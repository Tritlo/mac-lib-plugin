{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleInstances #-}
module MAC.Functor
   (
     -- Functor
       sfmap
     -- Applicative operator
     , (<<*>>)
     -- Relabeling
     , relabel
   )
where

import MAC.Lattice
import MAC.Core
import MAC.Labeled

-- | Labeled resources as functors
sfmap :: (a -> b) -> Labeled l a -> Labeled l b
sfmap = (<<*>>) . (MkRes . MkId)

-- Applicative operator (no pure)
(<<*>>)   :: Labeled l (a -> b) -> Labeled l a -> Labeled l b
f <<*>> x = MkRes $ MkId $ (unId (unRes f)) (unId (unRes x))

-- | It upgrades a labeled resource
relabel :: Less l l' => Labeled l a -> Labeled l' a
relabel = MkRes . unRes
