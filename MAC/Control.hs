{-# LANGUAGE Trustworthy #-}

-- | Provide primitives to communicate among family members. It provides an
--   API for sequential 'joinMAC' and concurrent ('forkMAC') setting
module MAC.Control
    (
       joinMAC     -- Secure communication for sequential programs
     , forkMAC     -- Spawing threads
     , forkMACMVar -- Returning futures
    )

where

import MAC.Lattice
import MAC.Core (MAC(),ioTCB,runMAC, Res (MkRes))
import MAC.Exception
import MAC.Labeled
import MAC.MVar

import Control.Exception
import Control.Concurrent

{-|
   Primitive which allows family members to safely communicate. The function
   finishes even if an exception is raised---the exception is rethrown when
   the returned value gets inspected.
   __This function must not be used in a concurrent setting__.
-}
joinMAC :: (Less l l') => MAC l' a -> MAC l (Labeled l' a)
joinMAC m = (ioTCB . runMAC)
              (catchMAC (m >>= safe_label) hd)
              where safe_label = return . MkRes . MkId
                    hd = safe_label . throw . proxy
                    proxy :: SomeException -> SomeException
                    proxy = id

{-
  Note:

  Instead of safe_label, it is possible to use the primitive label. In that
  manner, we do not break abstraction and we have more confidence about the
  correctness of the implementation. However, by doing so, the type signature
  needs to add Less l' l' into the type constraints.  Since that constraint
  always hold, it can be show that m >>= label and label (throw e) is equivalent
  to m >>= safe_label and safe_label (throw e) in joinMAC.
-}



-- | Safely spawning new threads
forkMAC :: Less l l' => MAC l' () -> MAC l ()
forkMAC m = (ioTCB . forkIO . runMAC) m >> return ()

{-|
   Safely spawning new threads. The function returns a labeled 'MVar' where
   the outcome of the thread is stored
-}
forkMACMVar :: (Less l' l', Less l l') => MAC l' a -> MAC l (MACMVar l' a)
forkMACMVar m = do lmv <- newMACEmptyMVar
                   forkMAC (m >>= putMACMVar lmv)
                   return lmv
