{--

2007 Formosan Summer School on Logic, Language, and Computation

Supplementary Haskell Code for 
  Introduction to Functional Program Derivation

Shin-Cheng Mu

--}

import List hiding (unfoldr, partition)


-- aliasing head and tail to be consistent with the lecture.

hd xs = head xs
tl xs = tail xs

-- iTree and eTree as defined in the lecture.

data ITree a = Null | Node a (ITree a) (ITree a) deriving Show
data ETree a = Tip a | Bin (ETree a) (ETree a)  deriving Show

square n = n*n

-- sum is predefined

-- sumsq = sum . map square

sumsq [] = 0 
sumsq (x:xs) = square x + sumsq xs

-- reverse is predefined

rcat xs ys = reverse xs ++ ys

-- ssp xs n = sumsq xs + n

ssp [] n = n
ssp (x:xs) n = ssp xs (square x + n)

steep [] = True
steep (x:xs) = steep xs && x > sum xs

steepsum [] = (True, 0)
steepsum (x:xs) = let (b,y) = steepsum xs
                  in (b && x > y, x + y) 

-- length is predefined
-- foldr is predefined

bmax x y | x >= y = x
         | otherwise = y
bmin x y | x >= y = y
         | otherwise = x

max = foldr bmax (-32767)

prod = foldr (*) 1

-- id, and are predefined
-- and = foldr (&&) True
-- id = foldr (:) []

-- takeWhile, dropWhile, inits, tails, and scanr are predefined

segs = concat . map inits . tails

-- mss = max . map sum . segs

mss = fst . foldr step (0,0)
  where step x (m,y) = ((0 `bmax` (x+y)) `bmax` m, 
                        0 `bmax` (x+y))

foldiT :: (a -> b -> b -> b) -> b -> ITree a -> b
foldiT f e Null = e
foldiT f e (Node a t u) =
  f a (foldiT f e t) (foldiT f e u)

foldeT :: (b -> b -> b) -> (a -> b) -> ETree a -> b
foldeT f g (Tip x) = g x
foldeT f g (Bin t u) =
  f (foldeT f g t) (foldeT f g u)
  
sizeiTree = foldiT (\x m n -> m + n + 1) 0

sumeTree = foldeT (+) id

flatteniT = foldiT (\x xs ys -> xs ++ [x] ++ ys) []
flatteneT = foldeT (++) (\x -> [x])

unfoldr :: (s -> Bool) -> (s -> (a,s)) -> s -> [a]
unfoldr p f s = 
   if p s then [] else 
      let (x,s') = f s in x : unfoldr p f s'              
      
split (f,g) a = (f a, g a)

fromto m = unfoldr (>= m) (split (id, (1+)))

tailsp = unfoldr null (split (id, tl))

from = unfoldr (const False) (split (id, (1+)))

-- iterate is predefined

merge xs = unfoldr null2 mrg xs
  where null2 (xs,ys) = null xs && null ys
        mrg ([], y:ys) = (y, ([],ys))
        mrg (x:xs, []) = (x, (xs,[]))
        mrg (x:xs, y:ys) = if x <= y then (x, (xs, y:ys))
                                     else (y, (x:xs, ys))

unfoldiT :: (a -> Bool) -> (a -> (b,a,a)) -> a -> ITree b
unfoldiT p f s = 
  if p s then Null
    else let (x,s1,s2) = f s
         in Node x (unfoldiT p f s1)
                   (unfoldiT p f s2)
                   
unfoldeT :: (a -> Bool) -> (a -> (a,a)) -> (a -> b) -> a -> ETree b
unfoldeT p f g s = 
  if p s then Tip (g s) 
    else let (s1,s2) = f s
         in Bin (unfoldeT p f g s1)
                (unfoldeT p f g s2)
                
single [x] = True
single xs = False

half = foldr step ([],[])
  where step x (xs,ys) = (ys, x:xs)
  
unflatteneT = unfoldeT single half id

-- Due to the "monomorphic restriction" of Haskell,
-- we have to eta-expand functions like msort, qsort,
-- isort, etc. That is, we cannot omit the xs in
--   msort xs = ... xs.

msort xs = (foldeT (curry merge) id . unflatteneT) xs

partition (x:xs) = (x, filter (<=x) xs, filter (>x) xs)

qsort xs = (flatteniT . unfoldiT null partition) xs

isort xs = foldr insert [] xs
  where insert x xs = takeWhile (<x) xs ++ [x] ++ dropWhile (<x) xs
  
ssort xs = unfoldr null select xs
  where select xs = let y = Main.max xs in (y, delete y xs)
  
hyloiT f e p g s = 
   if p s then e 
     else let (x, s1, s2) = g s
          in f x (hyloiT f e p g s1)
                 (hyloiT f e p g s2)


--- Exercises

-- 1.1.1

descend 0 = []
descend (n+1) = (n+1) : descend n

-- 1.1.2

mapiTree f Null = Null
mapiTree f (Node a t u) = Node (f a) (mapiTree f t) (mapiTree f u)

one x = 1

-- 1.1.3

mineTree (Tip x) = x
mineTree (Bin t u) = mineTree t `bmin` mineTree u

repeTree x (Tip y) = Tip x
repeTree x (Bin t u) = Bin (repeTree x t) (repeTree x u)

repbymin t = let m = mineTree t
             in repeTree m t

-- 1.2.3

swapiTree Null = Null
swapiTree (Node x t u) = Node x (swapiTree u) (swapiTree t)

-- 1.3.1

fact 0 = 0
fact (n+1) = (n+1) * fact n

-- 1.3.2

fib 0 = 0
fib 1 = 1
fib (n+2) = fib (n+1) + fib n

-- 2.1

flatten Null = []
flatten (Node x t u) = flatten t ++ [x] ++ flatten u

testTree1 = Node 4 (Node 2 (Node 1 Null Null)
                              (Node 3 Null Null))
                    (Node 5 Null
                              (Node 7 (Node 6 Null Null)
                                    Null))
-- 2.2

label Null n = Null
label (Node _ t u) n = Node n (label t (1 + n))
                              (label u (1 + n + sizeiTree t))

-- sizeiTree was defined using fold above. An equivalent definition
-- is here:

-- sizeiTree Null = 0
-- sizeiTree (Node x t u) = 1 + sizeiTree t + sizeiTree u

testTree2 = Node () (Node () (Node () Null Null)
                              (Node () Null Null))
                    (Node () Null
                              (Node () (Node () Null Null)
                                    Null))
