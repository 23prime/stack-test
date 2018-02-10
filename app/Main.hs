{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
import Algebra
import System.Random
import System.Environment
import Data.Time

--import ModifyTwit (run)
import IOAction
import Random (mkRandom)

main :: IO ()
main = mkRandom
  
{- evaluate EEA
main = do
  print $ s * a + t * b
  where
--    (_, s, t) = eea a b
    (_, s, t) = eeaAbs a b
    a = fibLog 100001
    b = fibLog 100000
-}
