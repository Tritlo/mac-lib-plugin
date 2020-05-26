{-# LANGUAGE Unsafe #-}

-- | It defines main data structures for security, i.e., monad family 'MAC' and
--   labeled resources 'Res'.
module MAC.Core
    (
     -- Resources definitions
       Res (MkRes,unRes)
     , labelOf
     -- Monad MAC
     , MAC (MkMAC)
     , runMAC
     -- IO actions into MAC
     , ioTCB
     -- Allow users to "promise" that something is true.
     , unsafeTrust
    )

where

import Control.Applicative
import Data.Coerce
import Data.Proxy

-- | Labeling expressions of type @a@ with label @l@.
newtype Res l a = MkRes {unRes :: a}

-- | Label of resources
labelOf :: Res l a -> Proxy l
labelOf _res = Proxy

{-|
    This monad labels the results of the computation (of type @a@) with
    label @l@.
-}
--newtype MAC l a = MkMAC {unMAC :: IO a}
newtype MAC l a = MkMAC (IO a)

instance Functor (MAC l) where
    fmap f (MkMAC io) = MkMAC (fmap f io)

instance Applicative (MAC l) where
    pure = MkMAC . return
    (<*>) (MkMAC f) (MkMAC a) = MkMAC (f <*> a)

instance Monad (MAC l) where
   return = pure
   MkMAC m >>= k = ioTCB (m >>= runMAC . k)


-- | It lifts arbitrary 'IO'-actions.
ioTCB :: IO a -> MAC l a
ioTCB = MkMAC
-- Should not be exported!

-- | Execute secure computations.
runMAC :: MAC l a -> IO a
runMAC (MkMAC m) = m

-- | we can already do this with the constructors anyway
unsafeTrust :: Res l a -> Res l' a
unsafeTrust = coerce