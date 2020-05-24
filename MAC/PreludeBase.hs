-- Copyright (c) 2020 Matthias Pall Gissurarson, Agustin Mista
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
module MAC.PreludeBase
  ( module MAC.PreludeBase
  , Bool(..), Int, Float, Char, Double, Integer, Rational, String, Ordering
  , fromString, MAC(), runMAC
  ) where

import Prelude (Bool, Int, Float, Char, Double, Integer, Rational, String, Ordering)
import qualified Prelude

import Data.String

import MAC.Core
import MAC.Labeled
import MAC.Lattice
import MAC.Control
import MAC.FlexibleLb

----------------------------------------
-- Res and Id are functors

instance Prelude.Functor (Res l) where
  fmap f (MkRes r) = MkRes (f r)

instance Prelude.Functor Id where
  fmap f (MkId r) = MkId (f r)

type LessMax2 m x y   = (m ~ Max2 x y, Less x m, Less y m)
type LessMax3 m x y z = (m ~ Max3 x y z, Less x m, Less y m, Less z m)

type UnOp     l       a b   = Labeled l a -> Labeled l b
type BinOp  x y m     a b c = LessMax2 m x y => Labeled x a -> Labeled y b -> Labeled m c
type TernOp x y z m a b c d = LessMax3 m x y z => Labeled x a -> Labeled y b -> Labeled z c -> Labeled m d


promote :: a -> Labeled l a
promote a = MkRes (MkId a)

unOp :: (a -> b) -> UnOp l a b
unOp = sfmap

binOp :: (a -> b -> c) -> BinOp x y m a b c
binOp f (MkRes (MkId a)) (MkRes (MkId b)) = MkRes (MkId (f a b))

ternOp :: (a -> b -> c -> d) -> TernOp x y z m a b c d
ternOp f (MkRes (MkId a)) (MkRes (MkId b)) (MkRes (MkId c)) = MkRes (MkId (f a b c))

-- This should not be exported
unUnOp :: UnOp l a b -> (a -> b)
unUnOp f = f'
  where f' a = (unId . unRes) (f (MkRes (MkId a)))

unBinOp :: LessMax2 m x y => BinOp x y m a b c -> (a -> b -> c)
unBinOp f = f'
  where f' a  b = (unId . unRes) (f (MkRes (MkId a)) (MkRes (MkId b)))

unTernOp :: LessMax3 m x y z => TernOp x y z m a b c d -> (a -> b -> c -> d)
unTernOp f = f'
  where f' a b c =
         (unId . unRes) (f (MkRes (MkId a)) (MkRes (MkId b)) (MkRes (MkId c)))


----------------------------------------
-- Boolean operators

true :: Labeled l Bool
true = promote Prelude.True

false :: Labeled l Bool
false = promote Prelude.False

(&&) :: BinOp x y m Bool Bool Bool
(&&) = binOp (Prelude.&&)

(||) :: BinOp x y m Bool Bool Bool
(||) = binOp (Prelude.||)

not :: UnOp l Bool Bool
not = unOp Prelude.not

----------------------------------------
-- Show / Read for labeled values

class Show a where
  show :: UnOp l a String

instance Prelude.Show a => Show a where
  show = unOp Prelude.show

instance (Prelude.Show a, Max2 l L ~ L ) => Prelude.Show (Labeled l a) where
  show (MkRes (MkId a)) = Prelude.show a

class Read a where
  read :: UnOp l String a

instance Prelude.Read a => Read a where
  read = unOp Prelude.read

----------------------------------------
-- Numeric instances

class Num a where
  (+)    :: BinOp x y m a a a
  (-)    :: BinOp x y m a a a
  (*)    :: BinOp x y m a a a
  negate :: UnOp l a a
  abs    :: UnOp l a a
  signum :: UnOp l a a

instance Prelude.Num a => Num a where
  (+)    = binOp (Prelude.+)
  (-)    = binOp (Prelude.-)
  (*)    = binOp (Prelude.*)
  negate = unOp Prelude.negate
  abs    = unOp Prelude.abs
  signum = unOp Prelude.signum

class Fractional a where
  (/)   :: BinOp x y m a a a
  recip :: UnOp l a a

instance Prelude.Fractional a => Fractional a where
  (/)   = binOp (Prelude./)
  recip = unOp Prelude.recip

class Prelude.Fractional a => Floating a  where
  pi      :: Labeled l a
  exp     :: UnOp l a a
  log     :: UnOp l a a
  sqrt    :: UnOp l a a
  (**)    :: BinOp x y m a a a
  logBase :: BinOp x y m a a a
  sin     :: UnOp l a a
  cos     :: UnOp l a a
  tan     :: UnOp l a a
  asin    :: UnOp l a a
  acos    :: UnOp l a a
  atan    :: UnOp l a a
  sinh    :: UnOp l a a
  cosh    :: UnOp l a a
  tanh    :: UnOp l a a
  asinh   :: UnOp l a a
  acosh   :: UnOp l a a
  atanh   :: UnOp l a a

instance (Prelude.Floating a, Prelude.Fractional a) => Floating a  where
  pi      = promote Prelude.pi
  exp     = unOp Prelude.exp
  log     = unOp Prelude.log
  sqrt    = unOp Prelude.sqrt
  (**)    = binOp (Prelude.**)
  logBase = binOp Prelude.logBase
  sin     = unOp Prelude.sin
  cos     = unOp Prelude.cos
  tan     = unOp Prelude.tan
  asin    = unOp Prelude.asin
  acos    = unOp Prelude.acos
  atan    = unOp Prelude.atan
  sinh    = unOp Prelude.sinh
  cosh    = unOp Prelude.cosh
  tanh    = unOp Prelude.tanh
  asinh   = unOp Prelude.asinh
  acosh   = unOp Prelude.acosh
  atanh   = unOp Prelude.atanh

class (Prelude.Num a, Prelude.Ord a) => Real a where
  toRational :: UnOp l a Rational

instance (Prelude.Num a, Prelude.Ord a, Prelude.Real a) => Real a where
  toRational = unOp Prelude.toRational

class (Prelude.Real a, Prelude.Enum a) => Integral a where
  quot      :: BinOp x y m a a a
  rem       :: BinOp x y m a a a
  div       :: BinOp x y m a a a
  mod       :: BinOp x y m a a a
  quotRem   :: BinOp x y m a a (a,a)
  divMod    :: BinOp x y m a a (a,a)
  toInteger :: UnOp l a Integer

instance (Prelude.Real a, Prelude.Enum a, Prelude.Integral a) => Integral a where
  quot      = binOp Prelude.quot
  rem       = binOp Prelude.rem
  div       = binOp Prelude.div
  mod       = binOp Prelude.mod
  quotRem   = binOp Prelude.quotRem
  divMod    = binOp Prelude.divMod
  toInteger = unOp Prelude.toInteger

class (Prelude.Real a, Prelude.Fractional a) => RealFrac a  where
  properFraction :: Prelude.Integral b => UnOp l a (b,a)
  truncate       :: Prelude.Integral b => UnOp l a b
  round          :: Prelude.Integral b => UnOp l a b
  ceiling        :: Prelude.Integral b => UnOp l a b
  floor          :: Prelude.Integral b => UnOp l a b

instance (Prelude.Real a, Prelude.Fractional a, Prelude.RealFrac a) => RealFrac a  where
  properFraction = unOp Prelude.properFraction
  truncate       = unOp Prelude.truncate
  round          = unOp Prelude.round
  ceiling        = unOp Prelude.ceiling
  floor          = unOp Prelude.floor

class (Prelude.RealFrac a, Prelude.Floating a) => RealFloat a  where
  floatRadix     :: UnOp l a Integer
  floatDigits    :: UnOp l a Int
  floatRange     :: UnOp l a (Int, Int)
  decodeFloat    :: UnOp l a (Integer, Int)
  encodeFloat    :: BinOp x y m Integer Int a
  exponent       :: UnOp l a Int
  significand    :: UnOp l a a
  scaleFloat     :: BinOp x y m Int a a
  isNaN          :: UnOp l a Bool
  isInfinite     :: UnOp l a Bool
  isDenormalized :: UnOp l a Bool
  isNegativeZero :: UnOp l a Bool
  isIEEE         :: UnOp l a Bool
  atan2          :: BinOp x y m a a a

instance (Prelude.RealFrac a, Prelude.Floating a, Prelude.RealFloat a) => RealFloat a  where
  floatRadix     = unOp Prelude.floatRadix
  floatDigits    = unOp Prelude.floatDigits
  floatRange     = unOp Prelude.floatRange
  decodeFloat    = unOp Prelude.decodeFloat
  encodeFloat    = binOp Prelude.encodeFloat
  exponent       = unOp Prelude.exponent
  significand    = unOp Prelude.significand
  scaleFloat     = binOp Prelude.scaleFloat
  isNaN          = unOp Prelude.isNaN
  isInfinite     = unOp Prelude.isInfinite
  isDenormalized = unOp Prelude.isDenormalized
  isNegativeZero = unOp Prelude.isNegativeZero
  isIEEE         = unOp Prelude.isIEEE
  atan2          = binOp Prelude.atan2

----------------------------------------
-- Eq / Ord

class Eq a where
  (==) :: BinOp x y m a a Bool
  (/=) :: BinOp x y m a a Bool

  (/=) x y =  unOp Prelude.not (x == y)

instance Prelude.Eq a => Eq a where
  (==) = binOp (Prelude.==)

class Prelude.Eq a => Ord a where
  compare :: BinOp x y m a a Ordering
  (<)     :: BinOp x y m a a Bool
  (<=)    :: BinOp x y m a a Bool
  (>)     :: BinOp x y m a a Bool
  (>=)    :: BinOp x y m a a Bool
  max     :: BinOp x y m a a a
  min     :: BinOp x y m a a a

instance (Prelude.Ord a, Prelude.Eq a) => Ord a where
  compare = binOp Prelude.compare
  (<)     = binOp (Prelude.<)
  (<=)    = binOp (Prelude.<=)
  (>)     = binOp (Prelude.>)
  (>=)    = binOp (Prelude.>=)
  max     = binOp Prelude.max
  min     = binOp Prelude.min


----------------------------------------
-- Enum / Bounded

class Enum a where
  succ           :: UnOp l a a
  pred           :: UnOp l a a
  toEnum         :: UnOp l Int a
  fromEnum       :: UnOp l a Int
  enumFrom       :: UnOp l a [a]
  enumFromThen   :: BinOp x y m a a [a]
  enumFromTo     :: BinOp x y m a a [a]
  enumFromThenTo :: TernOp x y z m a a a [a]

instance Prelude.Enum a => Enum a where
  succ           = unOp Prelude.succ
  pred           = unOp Prelude.pred
  toEnum         = unOp Prelude.toEnum
  fromEnum       = unOp Prelude.fromEnum
  enumFrom       = unOp Prelude.enumFrom
  enumFromThen   = binOp Prelude.enumFromThen
  enumFromTo     = binOp Prelude.enumFromTo
  enumFromThenTo = ternOp Prelude.enumFromThenTo

class Bounded a where
  minBound :: Labeled l a
  maxBound :: Labeled l a

instance Prelude.Bounded a => Bounded a where
  minBound = promote Prelude.minBound
  maxBound = promote Prelude.maxBound

----------------------------------------
-- Semigroup / Monoid

class Semigroup a where
  (<>) :: BinOp x y m a a a

instance Prelude.Semigroup a => Semigroup a where
  (<>) = binOp (Prelude.<>)

class Prelude.Semigroup a => Monoid a where
  mempty  :: Labeled l a
  mappend :: BinOp x y m a a a
  mconcat :: UnOp l [a] a

instance (Prelude.Semigroup a, Prelude.Monoid a) => Monoid a where
  mempty  = promote Prelude.mempty
  mappend = binOp Prelude.mappend
  mconcat = unOp Prelude.mconcat

----------------------------------------
-- Miscellaneous functions

id :: UnOp l a a
id =  unOp Prelude.id

const :: a -> b -> a
const = Prelude.const

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)

flip :: (a -> b -> c) -> (b -> a -> c)
flip f x y = f y x


class Prelude.Functor f => Liftable f where
    lift :: f (Labeled l a) -> Labeled l (f a)
    lift f = MkRes (MkId (Prelude.fmap (unId . unRes) f))

instance Liftable []
----------------------------------------
-- List functions
filter :: UnOp l a Bool -> UnOp l [a] [a]
filter f = unOp (Prelude.filter (unUnOp f))

map :: UnOp l a b -> UnOp l [a] [b]
map f = unOp (Prelude.map (unUnOp f))

head :: UnOp l [a] a
head = unOp Prelude.head

last :: UnOp l [a] a
last = unOp Prelude.last

tail :: UnOp l [a] [a]
tail = unOp Prelude.tail

init :: UnOp l [a] [a]
init = unOp Prelude.init

(!!) :: BinOp x y m [a] Int a
(!!) = binOp (Prelude.!!)
null :: Prelude.Foldable t => UnOp l (t a) Bool
null = unOp Prelude.null
length :: Prelude.Foldable t => UnOp l (t a) Int
length = unOp Prelude.length
reverse :: UnOp l [a] [a]
reverse = unOp Prelude.reverse
and :: Prelude.Foldable t => UnOp l (t Bool) Bool
and = unOp Prelude.and
or :: Prelude.Foldable t => UnOp l (t Bool) Bool
or = unOp Prelude.or
any :: Prelude.Foldable t => UnOp la a Bool -> UnOp la (t a) Bool
any = unOp . Prelude.any . unUnOp
all :: Prelude.Foldable t => UnOp la a Bool -> UnOp la (t a) Bool
all = unOp . Prelude.all . unUnOp
concat :: Prelude.Foldable t => UnOp l (t [a]) [a]
concat = unOp Prelude.concat
concatMap :: Prelude.Foldable t => UnOp l a [b] -> UnOp l (t a) [b]
concatMap = unOp . Prelude.concatMap . unUnOp
scanl :: LessMax2 m x y => BinOp x y m b a b -> BinOp x y m b [a] [b]
scanl = binOp . Prelude.scanl . unBinOp
-- We'd like this, but we can't prove that Max2 la la = la in the type system.
-- scanl1 :: BinOp la la la a a a -> UnOp la [a] [a]
-- scanl1 = unOp . Prelude.scanl1 . unBinOp
scanr :: LessMax2 m x y => BinOp x y m a b b -> BinOp x y m b [a] [b]
scanr = binOp . Prelude.scanr . unBinOp
iterate :: UnOp l a a -> UnOp l a [a]
iterate = unOp . Prelude.iterate . unUnOp
repeat :: UnOp l a [a]
repeat = unOp Prelude.repeat
replicate :: BinOp x y m Int a [a]
replicate = binOp Prelude.replicate
cycle :: UnOp l [a] [a]
cycle = unOp Prelude.cycle
take :: BinOp x y m Int [a] [a]
take = binOp Prelude.take
drop :: BinOp x y m Int [a] [a]
drop = binOp Prelude.drop
takeWhile :: UnOp l a Bool -> UnOp l [a] [a]
takeWhile = unOp . Prelude.takeWhile . unUnOp
dropWhile :: UnOp l a Bool -> UnOp l [a] [a]
dropWhile = unOp . Prelude.dropWhile . unUnOp
span :: UnOp l a Bool -> UnOp l [a] ([a], [a])
span = unOp . Prelude.span . unUnOp
break :: UnOp l a Bool -> UnOp l [a] ([a], [a])
break = unOp . Prelude.break . unUnOp
splitAt :: BinOp x y m Int [a] ([a], [a])
splitAt = binOp Prelude.splitAt
notElem :: (Prelude.Foldable t, Prelude.Eq a, LessMax2 lc la lb) => BinOp la lb lc a (t a) Bool 
notElem = binOp Prelude.notElem
lookup :: (Prelude.Eq a, LessMax2 lc la lb) => BinOp la lb lc a [(a, b)] (Prelude.Maybe b)
lookup = binOp Prelude.lookup

-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl :: (Prelude.Foldable t) => BinOp lb la lc b a b -> BinOp lb la lc b (t a) b
foldl = binOp . Prelude.foldl . unBinOp

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr :: (Prelude.Foldable t, LessMax2 lc la lb) => BinOp la lb lc a b b -> BinOp lb la lc b (t a) b
foldr = binOp . Prelude.foldr . unBinOp

----------------------------------------
-- Zipping and unzipping lists

zip :: BinOp x y m [a] [b] [(a, b)]
zip = binOp Prelude.zip

zip3 :: TernOp x y z m [a] [b] [c] [(a, b, c)]
zip3 = ternOp Prelude.zip3

zipWith :: (a -> b -> c) -> BinOp x y m [a] [b] [c]
zipWith f = binOp (Prelude.zipWith f)

zipWith3 :: (a -> b -> c -> d) -> TernOp x y z m [a] [b] [c] [d]
zipWith3 f = ternOp (Prelude.zipWith3 f)

unzip :: UnOp l [(a, b)] ([a], [b])
unzip = unOp Prelude.unzip

unzip3 :: UnOp l [(a, b, c)] ([a], [b], [c])
unzip3 = unOp Prelude.unzip3

----------------------------------------
-- String functions

lines :: UnOp l String [String]
lines = unOp Prelude.lines

words :: UnOp l String [String]
words = unOp Prelude.words

unlines :: UnOp l [String] String
unlines = unOp Prelude.unlines

unwords :: UnOp l [String] String
unwords = unOp Prelude.unwords

----------------------------------------
-- RebindableSyntax

instance IsString a => IsString (Labeled l a) where
  fromString = MkRes . MkId . fromString

instance (Prelude.Num a, a ~ Labeled l a) => Prelude.Num (Labeled l a) where
  fromInteger = MkRes . MkId . Prelude.fromInteger
  (+) a b = a + b
  (*) a b = a * b
  (-) a b = a - b
  negate = negate
  abs = abs
  signum = signum

instance (Prelude.Fractional a, a ~ Labeled l a) => Prelude.Fractional (Labeled l a) where
  fromRational = MkRes . MkId . Prelude.fromRational
  (/) a b = a / b

fromInteger :: Prelude.Num a => Integer -> a
fromInteger = Prelude.fromInteger

fromRational :: Prelude.Fractional a => Rational -> a
fromRational = Prelude.fromRational

ifThenElse :: TernOp lmax lb lt le Bool a a a
ifThenElse (MkRes (MkId Prelude.True))  t e = relabel t
ifThenElse (MkRes (MkId Prelude.False)) t e = relabel e

(>>=) :: Prelude.Monad m => m a -> (a -> m b) -> m b
(>>=) = (Prelude.>>=)

(>>) :: Prelude.Monad m => m a -> m b -> m b
(>>) = (Prelude.>>)

return :: Prelude.Monad m => a -> m a
return = Prelude.return
