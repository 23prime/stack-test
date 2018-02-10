{-# LANGUAGE ExistentialQuantification
           , ScopedTypeVariables
#-}

module Polynomial where

import Data.List
import Algebra

-- 1 + 2 x + x^3 = [1, 2, 0, 1]
data Poly a = (Show a, Eq a, Num a) => P [a]

instance Show (Poly a) where
  show (P f) = show $ killTailZero f

instance Eq (Poly a) where
  (P f) == (P g) = f == g

instance Num (Poly a) where
  (P f) + (P g) = P $ killTailZero $ zipWith' (+) f g
  (P f) - (P g) = P $ killTailZero $ zipWith' (+) f g
  (*)           = multiPoly 
  negate (P f)  = P $ map negate f
  abs (P f)     = P $ map abs f
  signum (P f)  = undefined
  fromInteger   = undefined

zipWith' :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith' _ f [] = f
zipWith' _ [] g = g
zipWith' p (x : xs) (y : ys) = p x y : (zipWith' p xs ys)

deg :: Poly a -> Int
deg (P f) = length $ killTailZero f

killTailZero :: (Num a, Eq a) => [a] -> [a]
killTailZero = reverse . snd . break (/= 0) . reverse

multiPoly :: Poly a -> Poly a -> Poly a
multiPoly (P f) (P g) = P $ map (mkList f g) [0 .. d]
  where
    df = deg (P f)
    dg = deg (P g)
    d  = max df dg
    l  = [(x, y) | x <- [0 .. df - 1], y <- [0 .. dg - 1]]
    mkList f g n = sum $ map (\x -> (f !! fst x) * (g !! snd x))
                   $ filter (\(x, y) -> x + y == n) l
