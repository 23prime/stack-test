module Random where

import System.IO
import System.Random
import System.Environment
import Data.Time


mkRandom :: IO ()
mkRandom = do
  s <- getArgs
  time <- getCurrentTime
  putStrLn $ s !! ((randomFromTime $ show time) `mod` length s)
  return ()

randamize :: Int -> Int
randamize = fst . random . mkStdGen

--getCurrentTime に対してランダムな整数を生成
randomFromTime :: String -> Int
randomFromTime time = let onlyTime = read $ (show time !!) <$> [12, 13, 15, 16, 18, 19] :: Int
                      in randamize onlyTime
