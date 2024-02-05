{-# LANGUAGE TypeOperators, TypeSynonymInstances,
             MultiParamTypeClasses, FlexibleInstances #-}

{- ver. 2018 -}

module MiniPrelude (
   module Prelude ,
   module MiniPrelude
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
         , (+), (-), (*), negate, abs, signum, (/), (^)
         , fromIntegral, truncate, round, ceiling, floor
         , properFraction
         , Monad(..), return, (>>), (>>=), fail)
import qualified Prelude
-- import GHC.Base hiding (Monad(..), (++), map)
import Data.Char

type List = []
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

infixl 7  *
infixl 6  +, -

(+), (-), (*) :: Int -> Int -> Int
(+) = (Prelude.+)
(-) = (Prelude.-)
(*) = (Prelude.*)

negate, abs, signum :: Int -> Int
negate = Prelude.negate
abs    = Prelude.abs
signum = Prelude.signum

infixl 7  *.
infixl 6  +. , -.

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

fromIntegral :: Int -> Float
fromIntegral = Prelude.fromIntegral

truncate, round, ceiling, floor :: Float -> Int
truncate = Prelude.truncate
round = Prelude.round
ceiling = Prelude.ceiling
floor = Prelude.floor

properFraction :: Float -> (Int, Float)
properFraction = Prelude.properFraction

-- monads

class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  (>>)   :: m a -> m b -> m b
  return :: a -> m a

  m >> n = m >>= const n

class Monad m => MonadFail m where
  fail :: m a

class MonadFail m => MonadExcept m where
  catch :: m a -> m a -> m a

class MonadFail m => MonadNondet m where
  mplus :: m a -> m a -> m a

{-
instance Monad Maybe where
  return = Just
  Just x  >>= k = k x
  Nothing >>= k = Nothing

instance MonadFail Maybe where
  fail = Nothing

instance MonadExcept Maybe where
  catch Nothing  h = h
  catch (Just x) h = Just x

{- Is this correct?-}

instance MonadNondet Maybe where
   Nothing `mplus` m = m
   Just x  `mplus` m = Just x

instance Monad List where
  return x = [x]
  xs >>= k = concat (map k xs)

instance MonadFail List where
  fail = []

instance MonadExcept List where
  catch [] h = h
  catch xs h = xs

instance MonadNondet List where
  mplus = (++)

-}
-- State

class Monad m => MonadState st m where
  get :: m st
  put :: st -> m ()

{-
newtype StFun st a = MkSF (st -> (a,st))

instance Monad (StFun st) where
  return x       = MkSF (\s -> (x,s))
  (MkSF f) >>= k = MkSF (\s -> let (x, s') = f s
                                   MkSF h  = k x
                               in h s')

instance MonadState st (StFun st) where
  get    = MkSF (\s -> (s,s))
  put s' = MkSF (\s -> ((),s'))
-}
