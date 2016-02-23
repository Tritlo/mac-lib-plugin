{-# LANGUAGE Trustworthy #-}
module MAC.Functor
   (
     -- Functor
       fmap
     -- Applicative functor
     , pure
     , (<*>)
     -- Relabeling
     , relabel
   )
where

import Control.Applicative

import MAC.Lattice
import MAC.Core

-- | Labeled resources as functors
instance Functor (Res l) where
    fmap f = MkRes . f . unRes

-- | Labeled resources as applicative functors
instance Applicative (Res l) where
    pure    = MkRes
    (<*>) f = MkRes . unRes f . unRes


-- | It upgrades a labeled resource
relabel :: Less l l' => Res l a -> Res l' a
relabel = MkRes . unRes
