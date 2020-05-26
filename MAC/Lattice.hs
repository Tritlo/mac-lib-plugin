{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Safe #-}

-- | Encodes a security lattice.
module MAC.Lattice
  ( Less ()
  , H ()
  , L ()
  , Max2
  , Max3
  , Same
  ) where

import GHC.TypeLits
import Data.Type.Bool

-- | Label for public data
data L
-- | Label for secrets
data H

-- | Type class encoding security lattices
class Less l l' where

instance Less L H where
instance Less l l where

-- | l should match l', but lazily.
type Same l l' = (Less l' l,  Less l l')

type family Max2 x y where
  Max2 H _ = H
  Max2 _ H = H
  Max2 l l = l

type family Max3 x y z where
  Max3 H _ _ = H
  Max3 _ H _ = H
  Max3 _ _ H = H
  Max3 l l l = l
