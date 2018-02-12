{-# LANGUAGE
DataKinds,
KindSignatures,
ScopedTypeVariables,
GADTs
#-}

module Polynomial where

import Data.List
import Data.Proxy
import GHC.TypeLits

-- My Modules.
--import Algebra
import Field

-- Type of Polynomial.
-- a : type of coefficient
-- ex. : P [1, (-2), 0, 1] = 1 - 2 x + x^3
data Poly a = (Show a, Eq a, Num a) => P [a]

-- Make Polynomial from Numerical List.
mkP :: (Show a, Eq a, Num a) => [a] -> Poly a
mkP xs
  | length xs == 1 = P xs
  | all (== 0) xs  = P [0]
  | otherwise      = P $ killTailZero xs

-- If last [a] == 0, then this 0 is unneeded.
-- ex. : [1, 1, 0] = 1 + x + 0x^2
killTailZero :: (Num a, Eq a) => [a] -> [a]
killTailZero [0] = [0]
killTailZero xs = reverse $ snd $ break (/= 0) $ reverse xs

-- Show Monomial; (a, Integer) : (coefficient, degree)
showMono :: (Show a, Eq a, Num a) => (a, Integer) -> String
showMono (c, 0) = show c -- degree == 0 => constant.
showMono (0, _) = ""     -- coefficient == 0 => empty.
showMono (1, 1) = "x"
showMono (1, d) = "x^" ++ show d
showMono (c, 1) = show c ++ "x"
showMono (c, d) = show c ++ "x^" ++ show d

-- Add Degree information to Polynomial List.
addDeg :: (Show a, Eq a, Num a) => Poly a -> [(a, Integer)]
addDeg (P ms) = zip ms [0 ..]

-- Add Sign information to Monomial String.
sign :: String -> String
sign a@(x : xs)
  | x == '-'  = " " ++ [x] ++ " " ++ xs
  | otherwise = " + " ++ a

-- Show Polynomial.
showPoly :: Poly a -> String
showPoly (P f)
  | length f' == 1 = show $ head f -- Then (P f) is also Monomial.
  | otherwise      = showMonos $ map showMono f''
  where
    f'                 = killTailZero f
    f''                = killHeadZero $ addDeg $ mkP f'
    showMonos (x : xs) = x ++ (concat $ map sign $ filter (/= "") $ xs)
    killHeadZero a@(x : xs) -- Unneeded init "0, 0,..." by Degree information.
      | fst x == 0 = killHeadZero xs 
      | otherwise  = a

instance Show (Poly a) where
  show = showPoly

instance Eq (Poly a) where
  (P f) == (P g) = all (== 0) $ zipWith (-) f g

instance Num (Poly a) where
  (P f) + (P g) = mkP $ zipWith' (+) f g
  (P f) - (P g) = mkP $ zipWith' (-) f g
  (*)           = multiPoly 
  negate (P f)  = P $ map negate f
  abs (P f)     = P $ map abs f
  signum (P f)  = undefined
  fromInteger m = undefined

-- Keep tail of longer List.
zipWith' :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith' _ f [] = f
zipWith' _ [] g = g
zipWith' p (x : xs) (y : ys) = p x y : (zipWith' p xs ys)

-- Degree of Polynomial.
deg :: Poly a -> Int
deg (P f) = (length $ killTailZero f) - 1

-- Multiplication of Polynomials.
multiPoly :: Poly a -> Poly a -> Poly a
multiPoly (P f) (P g) = mkP $ map (mkList f g) [0 .. d]
  where
    df = deg (P f)
    dg = deg (P g)
    d  = df + dg -- deg ((P f) * (P g)) <= df + dg
    l  = [(x, y) | x <- [0 .. df], y <- [0 .. dg]]
    mkList f g n
      = sum $ map (\x -> (f !! fst x) * (g !! snd x))
            $ filter (\(x, y) -> x + y == n) l
        -- Enumerate combinations Monomial in f or g which sum of each Degree is n.

-- Modulo of Polynomials.
-- Required that Coefficient Type is Fractional.
-- -> Because Polynomial Division require that Coefficients are on Field.
modPoly :: (Fractional a) => Poly a -> Poly a -> Poly a
modPoly (P f) (P g)
  | d < 0     = mkP f
  | otherwise = modPoly r (P g)
  where
    d = deg (P f) - deg (P g)
    q = mkP $ map (* ((last f) * (recip $ last g))) -- Determine Coefficient.
            $ replicate d 0 ++ [1]                  -- Determine Degree.
    r = (P f) - q * (P g)


-- Type of Finite Field F_4
newtype F4 = F4 (Poly (F 2))
  deriving (Eq)

-- Irreducible Polynomial on F_2.
irrPoly = mkP [1, 1, 1] :: Poly (F 2)

-- Make F_4 from Polinomial on F_2.
mkF4 (F4 p) = F4 $ p `modPoly` irrPoly

instance Show F4 where
  show (F4 f) = show $ f `modPoly` irrPoly

instance Num F4 where
  (F4 f) + (F4 g) = F4 $ (f + g) `modPoly` irrPoly
  (F4 f) - (F4 g) = F4 $ (f - g) `modPoly` irrPoly
  (F4 f) * (F4 g) = F4 $ (f * g) `modPoly` irrPoly
  negate (F4 f)   = F4 $ negate f
  abs (F4 f)      = F4 $ abs f
  signum (F4 f)   = undefined
  fromInteger m   = undefined
