{-# LANGUAGE TypeOperators, FunctionalDependencies #-}

{- ver. 2017,
   modified for FLOLAC 2024
    * kept the Num class -}

module MiniPrelude (
   module Prelude ,
   module MiniPrelude ,
   module Control.Monad
  ) where
-- import GHC.Types hiding ((:))
-- import qualified GHC.Types as GTypes
import Prelude
  hiding ( -- lists
           head, tail, init, last, (++)
         , take, drop, takeWhile, dropWhile
         , concat, concatMap, foldr, foldr1
         , foldl, foldl1, map, filter, zip, unzip
         , null, length, sum, product, elem, notElem
         , maximum, minimum
         , and, or, any, all
         -- arithmetics
         -- , (+), (-), (*), negate, abs, signum, (/), (^)
         , fromIntegral, truncate, round, ceiling, floor
         , properFraction
         , MonadFail, fail
         )
import qualified Prelude
import Data.Char
import Control.Monad (liftM, ap)

type List a = [a]
type a :* b = (a , b)
type Nat = Int

-- Lists

-- cons :: a -> List a -> List a
-- cons = (GTypes.:)

head :: List a -> a
head = Prelude.head

tail :: List a -> List a
tail = Prelude.tail

init :: List a -> List a
init = Prelude.init

last :: List a -> a
last = Prelude.last

(++) :: List a -> List a -> List a
(++) = (Prelude.++)

concat :: List (List a) -> List a
concat = Prelude.concat

concatMap :: (a -> List b) -> List a -> List b
concatMap = Prelude.concatMap

take :: Int -> List a -> List a
take = Prelude.take

drop :: Int -> List a -> List a
drop = Prelude.drop

takeWhile :: (a -> Bool) -> List a -> List a
takeWhile = Prelude.takeWhile

dropWhile :: (a -> Bool) -> List a -> List a
dropWhile = Prelude.dropWhile

filter :: (a -> Bool) -> List a -> List a
filter = Prelude.filter

zip :: List a -> List b -> List (a, b)
zip = Prelude.zip

unzip :: List (a, b) -> (List a, List b)
unzip = Prelude.unzip

map :: (a -> b) -> List a -> List b
map = Prelude.map

foldr :: (a -> b -> b) -> b -> List a -> b
foldr = Prelude.foldr

foldr1 :: (a -> a -> a) -> List a -> a
foldr1 = Prelude.foldr1

foldl :: (b -> a -> b) -> b -> List a -> b
foldl = Prelude.foldl

foldl1 :: (a -> a -> a) -> List a -> a
foldl1 = Prelude.foldl1

null :: List a -> Bool
null = Prelude.null

length :: List a -> Int
length = Prelude.length

sum :: Num a => List a -> a
sum = Prelude.sum

product :: Num a => List a -> a
product = Prelude.product

elem :: Eq a => a -> List a -> Bool
elem = Prelude.elem

notElem :: Eq a => a -> List a -> Bool
notElem = Prelude.notElem

maximum :: Ord a => List a -> a
maximum = Prelude.maximum

minimum :: Ord a => List a -> a
minimum = Prelude.minimum

and :: List Bool -> Bool
and = Prelude.and

or :: List Bool -> Bool
or = Prelude.or

any :: (a -> Bool) -> List a -> Bool
any = Prelude.any

all :: (a -> Bool) -> List a -> Bool
all = Prelude.all

-- Arithmetics
{-
infixl 7 *, /
infixl 6 +, -

(+), (-), (*) :: Int -> Int -> Int
(+) = (Prelude.+)
(-) = (Prelude.-)
(*) = (Prelude.*)

negate, abs, signum :: Int -> Int
negate = Prelude.negate
abs    = Prelude.abs
signum = Prelude.signum

infixl 7 *., /.
infixl 6 +., -.

(+.), (-.), (*.) :: Float -> Float -> Float
(+.) = (Prelude.+)
(-.) = (Prelude.-)
(*.) = (Prelude.*)

(/), (/.) :: Float -> Float -> Float
(/)  = (Prelude./)
(/.) = (Prelude./)

infixr 8 ^, ^.

(^) :: Int -> Int -> Int
(^) = (Prelude.^)

(^.) :: Float -> Int -> Float
(^.) = (Prelude.^)
-}

fromIntegral :: Int -> Float
fromIntegral = Prelude.fromIntegral

truncate, round, ceiling, floor :: Float -> Int
truncate = Prelude.truncate
round = Prelude.round
ceiling = Prelude.ceiling
floor = Prelude.floor

properFraction :: Float -> (Int, Float)
properFraction = Prelude.properFraction

-- Things about Monads

class Monad m => MonadFail m where
  fail  :: m a

class MonadFail m => MonadCatch m where
  catch :: m a -> m a -> m a

class Monad m => MonadExcept e m | m -> e where
  throw :: e -> m a
  catchE :: m a -> (e -> m a) -> m a

class Monad m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

class Monad m => MonadReader e m | m -> e where
  ask   :: m e
  local :: (e -> e) -> m a -> m a

class Monad m => MonadAlt m where
  (<|>)  :: m a -> m a -> m a

class (MonadFail m, MonadAlt m) => MonadNondet m where
