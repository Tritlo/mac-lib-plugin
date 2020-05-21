{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}

-- | Encodes a security lattice.
module MAC.Lattice
  ( Less ()
  , H ()
  , L ()
  , Max2
  , Max3
  ) where

import GHC.TypeLits
import Data.Type.Bool

-- | Label for public data
data L
-- | Label for secrets
data H

-- | Type class encoding security lattices
class Less l l' where

instance Less L L where
instance Less L H where
instance Less H H where

#ifndef USE_PLUGIN
instance TypeError (Text "Found forbidden flow from H to L") => Less H L where
#else
instance Less H L where
#endif

type family Max2 x y where
  Max2 H _ = H
  Max2 _ H = H
  Max2 _ _ = L

type family Max3 x y z where
  Max3 H _ _ = H
  Max3 _ H _ = H
  Max3 _ _ H = H
  Max3 _ _ _ = L
