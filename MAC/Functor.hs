{-# LANGUAGE Trustworthy #-}
module MAC.Functor
   (
     -- Functor
       fmap
     -- Applicative operator
     , (<<*>>)
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

-- Applicative operator (no pure)
(<<*>>) :: Res l (a -> b) -> Res l a -> Res l b
(<<*>>) f = MkRes . unRes f . unRes

-- | It upgrades a labeled resource
relabel :: Less l l' => Res l a -> Res l' a
relabel = MkRes . unRes
