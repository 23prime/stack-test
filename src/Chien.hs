module Chien where

test :: [Int] -> Int
test [] = 0
test ~(x : xs) = x + test xs

f = g f
g ~(x : xs) = 1 : xs
main = print $ take 3 f
