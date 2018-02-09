{-# LANGUAGE DataKinds
           , KindSignatures
           , ScopedTypeVariables
           , ExistentialQuantification
           , DeriveFunctor
           , StandaloneDeriving
           , GADTs
#-}

module Algebra where

import Control.Monad.Free
import Data.Type.Equality
import Data.Type.Bool
import Data.Proxy
import GHC.TypeLits
import System.Random

class Eq r => Ring r where
  (+.)    :: r -> r -> r
  rNegate :: r -> r
  (-.)    :: r -> r -> r
  zero    :: r
  (*.)    :: r -> r -> r
  one     :: r

class Ring r => EuclidRing r where
  rDiv    :: r -> r -> r
  rMod    :: r -> r -> r
  rDivMod :: r -> r -> (r, r)

class Num k => Field k where
  fRecip :: k -> Maybe k

newtype F (p :: Nat) = F Integer
  deriving (Eq, Ord)

--characteristic :: KnownNat p => F p -> Integer
--characteristic = natVal

mkF :: KnownNat p => Integer -> F p
mkF n
  | isPrime $ natVal r = r
  | otherwise          = error "Required that p is prime."
  where
    r = F $ n `mod` natVal r

instance  KnownNat p => Show (F p) where
  show (F m) = show m

instance KnownNat p => Num (F p) where
  (F m) + (F n) = mkF $ m + n
  (F m) - (F n) = mkF $ m - n
  (F m) * (F n) = mkF $ m * n
  negate (F m)  = mkF $ - m
  abs           = id
  signum (F m)
    | m == 0    = F 0
    | otherwise = F 1
  fromInteger   = mkF

instance KnownNat p => Field (F p) where
  fRecip a@(F m)
    | (mkF m :: F p) == 0 = Nothing
    | isPrime p           = Just $ mkF s
    | otherwise           = Nothing
    where
      (_, s, _) = eea m p
      p = natVal a

instance KnownNat p => Enum (F p) where
  succ (F m)     = mkF $ succ m
  pred (F m)     = mkF $ pred m
  toEnum m       = mkF $ toInteger m
  fromEnum a@(F m)
    | isPrime p = fromInteger $ m `mod` p
    | otherwise = undefined    
    where
      p = natVal a

instance KnownNat p => Real (F p) where
  toRational (F m) = toRational m

instance KnownNat p => Integral (F p) where
  a@(F m) `quot` (F n)
    | (mkF m :: F p) == 0 = undefined
    | otherwise           = let (_, s, _) = eea n p
                            in  mkF $ m * s
    where
      p = natVal a
  rem _ _ = F 0
  div = quot
  mod _ _ = F 0
  (F m) `quotRem` (F n) = ((F m) `quot` (F n), F 0)
  divMod = quotRem
  toInteger (F m) = m

-- EEA
eea :: (Integral m) => m -> m -> (m, m, m)
eea f g = loop (f, 1, 0) (g, 0, 1)
  where
    loop (r0, s0, t0) (r1, s1, t1)
      | r2 == 0   = (sg * r1, sg * s1, sg * t1)
      | otherwise = loop (r1, s1, t1) (r2, s2, t2)
      where
        sg      = signum r1
        (q, r2) = r0 `divMod` r1
        s2      = s0 - s1 * q
        t2      = t0 - t1 * q

-- 途中経過も出力
eeaList :: (Integral m) => m -> m -> [(m, m, m)]
eeaList f g = loop (f, 1, 0) (g, 0, 1)
  where
    loop (r0, s0, t0) (r1, s1, t1)
      | r2 == 0   = [(r0, s0, t0)] ++ [(sg * r1, sg * s1, sg * t1)]
      | otherwise = [(r0, s0, t0)] ++ loop (r1, s1, t1) (r2, s2, t2)
      where
        sg      = signum r1
        (q, r2) = r0 `divMod` r1
        s2      = s0 - s1 * q
        t2      = t0 - t1 * q

-- 余りの絶対値を小さくとる割り算
divModAbs :: (Integral m) => m -> m -> (m, m)
divModAbs a b
  | abs b >= abs (2 * r) = (q, r)
  | otherwise            = (q + 1, r - b)
  where
    (q, r) = a `divMod` b

-- EEA with absDivMod
eeaAbs :: (Integral m) => m -> m -> (m, m, m)
eeaAbs f g = loop (f, 1, 0) (g, 0, 1)
  where
    loop (r0, s0, t0) (r1, s1, t1)
      | r2 == 0   = (sg * r1, sg * s1, sg * t1)
      | otherwise = loop (r1, s1, t1) (r2, s2, t2)
      where
        sg      = signum r1
        (q, r2) = r0 `divModAbs` r1
        s2      = s0 - s1 * q
        t2      = t0 - t1 * q

absEeaList :: (Integral m) => m -> m -> [(m, m, m)]
absEeaList f g = loop (f, 1, 0) (g, 0, 1)
  where
    loop (r0, s0, t0) (r1, s1, t1)
      | r2 == 0   = [(r0, s0, t0)] ++ [(sg * r1, sg * s1, sg * t1)]
      | otherwise = [(r0, s0, t0)] ++ loop (r1, s1, t1) (r2, s2, t2)
      where
        sg      = signum r1
        (q, r2) = r0 `divModAbs` r1
        s2      = s0 - s1 * q
        t2      = t0 - t1 * q

-- EEA の検算
checkEea :: (Integral m, Random m) => (m -> m -> (m, m, m)) -> Bool
checkEea f
  | False `elem` checkedList = False
  | otherwise                 = True
  where
    checkedList = check f <$> as <*> bs
    as = map (fst . random . mkStdGen) [0..99]
    bs = map (fst . random . mkStdGen) [0..(-99)]
    check f a b
      | let (_, s, t) = f a b
        in  s * a + t * b == gcd a b = True
      | otherwise                    = False

-- EEA の反復回数比較
-- (通常版，調整済み)
eeaFibIteration :: Int -> (Int, Int)
eeaFibIteration a = (length e, length e')
  where
    (s, t) = (fibLog a, fibLog $ a + 1)
    e  = eeaList s t
    e' = absEeaList s t

-------------------------------------------------------------------------------------

isPrime :: (Integral m) => m -> Bool
isPrime n
  | n <  2    = False
  | n == 2    = True
  | even n    = False
  | otherwise = check n 3
  where
    check n m
      | n < m ^ 2      = True
      | n `mod` m == 0 = False
      | otherwise      = check n (m + 2)

factors :: (Integral m) => m -> [m]
factors n = [x | x <- [1..n], n `mod` x == 0]

factorize :: (Integral m) => m -> [m]
factorize 1 = []
factorize x = v : factorize (x `div` v)
  where
    v = (factors x) !! 1

isPrimePower :: (Integral m) => m -> Bool
isPrimePower n
  | n < 2                          = False
  | length xs == 1                 = False
  | all (== hxs) xs && isPrime hxs = True
  | otherwise                      = False
    where
      hxs = head xs
      xs  = factorize n

primePower :: (Integral m) => m -> Maybe (m, Int)
primePower n
  | n < 2                          = Nothing
  | length xs == 1                 = Nothing
  | all (== hxs) xs && isPrime hxs = Just (hxs, length xs)
  | otherwise                      = Nothing
    where
      hxs = head xs
      xs  = factorize n

-------------------------------------------------------------------------------------

fibList = 1 : 1 : zipWith (+) fibList (tail fibList)

fib = mkFib 0 1
  where
    mkFib _ y 0 = y
    mkFib x y k = mkFib y (x + y) (k - 1)

fibT = snd . mkFib
  where
    mkFib 0 = (0, 1)
    mkFib n = let (a, b) = mkFib (n - 1)
              in  (b, a + b)

fibLog :: Int -> Integer
fibLog n = fst $ loop (n + 1)
  where
    loop 0 = (0, 1)
    loop n
      | odd n     = let (a, b) = loop (n `div` 2)
                        c      = a + b
                    in (a ^ 2 + b ^ 2, a * b + b * c)
      | otherwise = let (a, b) = loop (n - 1)
                    in (b, a + b)

-------------------------------------------------------------------------------------

primes = [2..] \\ composites

composites = mergeAll multiples

multiples = [map (n *) [n..] | n <- [2..]]

(x : xs) \\ (y : ys)
  | x < y  = x : (xs \\ (y : ys))
  | x == y = xs \\ ys
  | x > y  = (x : xs) \\ ys

merge :: Ord a => [a] -> [a] -> [a]
merge (x : xs) (y : ys)
  | x < y  = x : (merge xs (y : ys))
  | x == y = merge xs ys
  | x > y  = merge (x : xs) ys

xmerge (x : xs) ys = x : merge xs ys

mergeAll = foldr1 xmerge
