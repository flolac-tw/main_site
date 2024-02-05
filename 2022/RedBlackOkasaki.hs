{-# LANGUAGE StandaloneDeriving #-}

-- Code adapted from Chris Okasaki,
--  Red-black trees in a functional setting.
--  Journal of Functional Programming, 9(4):471–477, 1999.

-- Uncomment this if you manage to have QuickCheck installed.
-- import Test.QuickCheck

data RBTree a = E | R (RBTree a) a (RBTree a)
                  | B (RBTree a) a (RBTree a)
data Color    = Red | Blk

type Nat = Int

insert :: Int -> RBTree Int -> RBTree Int
insert k t = blacken (ins k t)
  where blacken (R t x u) = B t x u
        blacken t = t

ins :: Int -> RBTree Int -> RBTree Int
ins k E = R E k E
ins k (R t x u) | k <  x = R (ins k t) x u
                | k == x = R t x u
                | k >  x = undefined
ins k (B t x u) | k <  x = undefined
                | k == x = B t x u
                | k >  x = balance t x (ins k u)

balance :: RBTree a -> a -> RBTree a -> RBTree a
balance (R (R t x u) y v) z w = undefined
balance (R t x (R u y v)) z w = undefined
balance t x (R (R u y v) z w) = undefined
balance t x (R u y (R v z w)) = undefined
balance t x u = B t x u

-- height

bheight :: RBTree a -> Nat
bheight E = 0
bheight (R t x u) = bheight t `max` bheight u
bheight (B t x u) = 1 + (bheight t `max` bheight u)

isBalanced :: RBTree a -> Bool
isBalanced E = True
isBalanced (R t x u) = bheight t == bheight u &&
                       isBalanced t && isBalanced u
isBalanced (B t x u) = bheight t == bheight u &&
                       isBalanced t && isBalanced u

-- sortedness

maxRBT :: RBTree Int -> Int
maxRBT E = minBound
maxRBT (R t x u) = undefined
maxRBT (B t x u) = undefined

minRBT :: RBTree Int -> Int
minRBT E = maxBound
minRBT (R t x u) = undefined
minRBT (B t x u) = undefined

isSorted :: RBTree Int -> Bool
isSorted E = True
isSorted (R t x u) = undefined
isSorted (B t x u) = undefined
-- colors

color :: RBTree a -> Color
color E = Blk
color (R _ _ _) = Red
color (B _ _ _) = Blk

isRB :: RBTree a -> Bool
isRB E = True
isRB (B t x u) = isRB t && isRB u
isRB (R t x u) = color t == Blk && color u == Blk &&
                   isRB t && isRB u

isBlkRB :: RBTree a -> Bool
isBlkRB t = color t == Blk && isRB t

  -- infrared

isIRB :: RBTree a -> Bool
isIRB E = True
isIRB (B t x u) = isRB t && isRB u
isIRB (R t x u) = (color t == Blk || color u == Blk) &&
                    isRB t && isRB u

-- properties.
-- To test these properties, type for example,
--   > quickCheck propIsBalanced
-- in GHCi.

propIsBalanced :: [Int] -> Bool
propIsBalanced = isBalanced . foldr insert E

propIsSorted :: [Int] -> Bool
propIsSorted = isSorted . foldr insert E

propIsBlkRB :: [Int] -> Bool
propIsBlkRB = isBlkRB . foldr insert E

propIsBlkRB' :: Int -> [Int] -> Bool   -- would be falisified
propIsBlkRB' x xs = isBlkRB . ins x . foldr insert E $ xs

propIsIRB :: Int -> [Int] -> Bool
propIsIRB x xs = isIRB . ins x . foldr insert E $ xs

-- deriving instances

deriving instance Eq a => Eq (RBTree a)
deriving instance Show a => Show (RBTree a)
deriving instance Eq Color
deriving instance Show Color
-- deriving instance Arbitrary a => Arbitrary (RBTree a)
