module Util.List where

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn s xs = cons $ case break (== s) xs of
    (l, s') -> (l, case s' of
        []      -> []
        _:s''   -> splitOn s s'')
    where
        cons ~(h, t)        =  h : t

joinOn :: a -> [[a]] -> [a]
joinOn _ []     = []
joinOn _ [x]    = x
joinOn s (x:xs) = x ++ s : joinOn s xs
