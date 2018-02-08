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

data F (p :: Nat) = KnownNat p => F (Proxy p) Integer

instance  KnownNat p => Show (F p) where
  show (F p m)
    | isPrime p' = show $ m `mod` p' -- 素体（位数が素数）の時のみ値を返す
    | otherwise  = undefined
    where p' = natVal p

instance KnownNat p => Eq (F p) where
  F p m == F q n
    | isPrime p' = (m - n) `mod` p' == 0
    | otherwise  = undefined
    where p' = natVal p

instance KnownNat p => Ord (F p) where
  compare (F p m) (F q n)
    | isPrime p' = compare (m `mod` p') (n `mod` p')
    | otherwise  = undefined    
    where p' = natVal p

instance KnownNat p => Num (F p) where
  (F p m) + (F q n) = F p $ m + n
  (F p m) - (F q n) = F p $ m - n
  (F p m) * (F q n) = F p $ m * n
  negate (F p m)    = F p $ - m
  abs               = id
  signum (F p m)
    | m == 0        = F p 0
    | otherwise     = F p 1
  fromInteger m     = let p = Proxy :: Proxy p
                      in  F p $ m `mod` natVal p

instance KnownNat p => Field (F p) where
  fRecip (F p m)
    | m `mod` p' == 0 = Nothing -- 0 の逆元は存在しない
    | isPrime p'      = Just $ F p s
    | otherwise       = Nothing
    where (r, s, _) = eea m p'
          p'        = natVal p

instance KnownNat p => Enum (F p) where
  succ (F p m)     = F p $ succ m
  pred (F p m)     = F p $ pred m
  toEnum m         = F (Proxy :: Proxy p) $ toInteger m
  fromEnum (F p m)
    | isPrime p' = fromInteger $ m `mod` p'
    | otherwise  = undefined    
    where p' = natVal p


-- 型変換
toF :: KnownNat p => Proxy p -> Integer -> F p
toF p = fromInteger

-- 異なる位数の共存
--fieldList :: forall p. [F p]
--fieldList = [F (Proxy :: Proxy 5) 9, F (Proxy :: Proxy 7) 9]

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
