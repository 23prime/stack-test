{-# LANGUAGE DataKinds
           , KindSignatures
           , ScopedTypeVariables
           , ExistentialQuantification
           , DeriveFunctor
           , StandaloneDeriving
           , ConstraintKinds
           , PolyKinds
           , FunctionalDependencies
           , TypeFamilies
           , TypeFamilyDependencies
           , TypeOperators
           , GADTs
           , UndecidableInstances
           , RankNTypes
#-}

module Practice where

import Control.Exception
import Control.Monad
import Control.Monad.Writer
import Control.Monad.ST
import Control.DeepSeq
import System.IO
import Data.List
import Data.Type.Equality
import Data.Type.Bool
import Data.Proxy
import GHC.TypeLits
--import Algebra

type Stringy a = (Show a, Read a)


main :: IO ()
main = do
  let loop :: Int -> IO ()
      loop i = when (i < 10) $ do
        print i
        loop $ i + 1
  loop 0


-- TypeFamilies
data family FC a
data instance FC Int   = A
data instance FC Float = B

type family FC' a where
  FC' Int   = Integer
  FC' Float = Double

type family F a = r | r -> a where
  F Int  = Bool
  F Char = String

f :: (Bounded b) => F b -> b
f x = maxBound

type family Add' (a :: Nat) (b :: Nat) = (c :: Nat) where
  Add' a b = a + b
  
g :: a -> (a ~ Int) => Integer
g = toInteger

type family (x :: Nat) % (y :: Nat) :: Nat  where
  x % y = Mod' x y (y <=? x)

type family Mod' (x :: Nat) (y :: Nat) (xGeqY :: Bool) :: Nat where
  Mod' x y 'True  = Mod' (x - y) y (y <=? (x - y))
  Mod' x y 'False = x
