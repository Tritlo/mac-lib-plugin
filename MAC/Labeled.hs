{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}

-- | Labeled expressions.
module MAC.Labeled
  ( Labeled ()
  , Id (MkId, unId)
  , label
  , unlabel
  , Secret
  , Public
  ) where

import MAC.Lattice
import MAC.Core (MAC(), Res())
import MAC.Effects

-- | Type denoting values of type @a@
newtype Id a = MkId { unId :: a }

-- | Labeled expressions
type Labeled l a = Res l (Id a)

-- | Creation of labeled expressions
label :: Less l l' => a -> MAC l (Labeled l' a)
label = create . return . MkId

-- | Observing labeled expressions
unlabel :: Less l' l => Labeled l' a -> MAC l a
unlabel = readdown (return . unId)

type Secret a = Labeled H a
type Public a = Labeled L a
