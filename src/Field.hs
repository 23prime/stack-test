{-# LANGUAGE
DataKinds,
KindSignatures,
ScopedTypeVariables
#-}

module Field where

import Data.Proxy
import GHC.TypeLits

import Algebra (eea, isPrime)

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
  order  :: k -> Integer

newtype F (p :: Nat) = F Integer
  deriving (Eq, Ord)

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
  order a@(F m) = natVal a

instance KnownNat p => Enum (F p) where
  succ (F m)     = mkF $ succ m
  pred (F m)     = mkF $ pred m
  toEnum m       = mkF $ toInteger m
  fromEnum a@(F m)
    | isPrime p = fromInteger $ m `mod` p
    | otherwise = undefined    
    where
      p = order a

instance KnownNat p => Real (F p) where
  toRational (F m) = toRational m

instance KnownNat p => Integral (F p) where
  a@(F m) `quot` (F n)
    | (mkF m :: F p) == 0 = undefined
    | otherwise           = let (_, s, _) = eea n p
                            in  mkF $ m * s
    where
      p = order a
  _ `rem` _ = F 0
  div = quot
  _ `mod` _ = F 0
  (F m) `quotRem` (F n) = ((F m) `quot` (F n), F 0)
  divMod = quotRem
  toInteger (F m) = m

instance KnownNat p => Fractional (F p) where
  recip a@(F m)
    | (mkF m :: F p) == 0 = undefined
    | isPrime p           = mkF s
    | otherwise           = undefined
    where
      (_, s, _) = eea m p
      p = natVal a
  (F m) / (F n) = (F m) * recip (F n)
  fromRational = undefined
