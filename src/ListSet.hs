module ListSet where

import Data.List

--------------------------------------------------------------------------------
-- Set operation
--------------------------------------------------------------------------------
setify :: Ord a => [a] -> [a]
setify = uniq . sort
  where
    uniq [] = []
    uniq (x:xs) = x : uniq (dropWhile (== x) xs)
{-
uniq_ xs = uniqSub [] xs
    where
        uniqSub us [] = reverse us
        uniqSub us (x:xs) | x `elem` us = uniqSub us xs
                          | otherwise = uniqSub (x:us) xs
-}
setdiff xs [] = xs
setdiff [] _ = []
setdiff (x:xs) (y:ys) | x < y  = x : (setdiff xs (y:ys))
                      | x == y = setdiff xs ys
                      | x > y  = setdiff (x:xs) ys

setadd x [] = [x]
setadd x (y:ys) | y < x  = y : (setadd x ys)
                | y == x = (y:ys)
                | y > x  = (x:y:ys)

intersection [] _ = []
intersection _ [] = []
intersection (x:xs) (y:ys) | x < y  = intersection xs (y:ys)
                           | x == y = x : (intersection xs ys)
                           | x > y  = intersection (x:xs) ys
setUnion [] xs = xs
setUnion xs [] = xs
setUnion (x:xs) (y:ys) | x < y  = x : setUnion xs (y:ys)
                       | x == y = x : setUnion xs ys
                       | x > y  = y : setUnion (x:xs) ys

-- Cartesian product, sorted
setprod xs ys = concatMap (\x -> map (\y -> (x, y)) ys) xs

-- not sorted
subsets [] = [[]]
subsets (x:xs) = map (x :) xss ++ xss where xss = subsets xs

-- sorted
nonEmptySubsets [] = []
nonEmptySubsets [x] = [[x]]
nonEmptySubsets (x:xs) = map (x :) xss ++ xss where xss = nonEmptySubsets xs

isSubset [] _ = True
isSubset (x:xs) [] = False
isSubset (x:xs) (y:ys) | x < y  = False
                       | x == y = isSubset xs ys
                       | x > y  = isSubset (x:xs) ys

isProperSubset [] [] = False
isProperSubset [] (y:ys) = True
isProperSubset (x:xs) [] = False
isProperSubset (x:xs) (y:ys) | x < y  = False
                             | x == y = isProperSubset xs ys
                             | x > y  = isSubset (x:xs) ys

