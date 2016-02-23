{-# LANGUAGE Trustworthy #-}

-- | A safe interface for module @Core.hs@
module MAC.MAC
    (
     -- It comes from Core
       Res ()
     , labelOf
     -- Monad MAC
     , MAC ()
     , runMAC
     -- Labeled values
     , fmap
     , pure
     , (<*>)
     , relabel
     -- Auxiliary proxies
     , fix
    )

where

import MAC.Core
import MAC.Functor

-- | To help the type-system
fix :: l -> MAC l ()
fix _l = return ()
