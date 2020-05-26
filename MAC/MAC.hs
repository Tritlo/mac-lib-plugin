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
     -- Auxiliary proxies
     , fix
    )

where

import MAC.Core
import MAC.Effects (fix)