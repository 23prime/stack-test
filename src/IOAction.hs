module IOAction where

import System.IO
import System.Environment


run :: IO ()
run = do
  files <- getArgs
  let file = head files
  withFile file ReadMode $ \text' -> do
    text <- hGetContents text'
    let killed = killEndSpace text
    writeFile ("result-" ++ file) killed


killEndSpace :: String -> String
killEndSpace s = init $ unlines $ map kill $ lines s
  where
    xs = lines s
    kill :: String -> String
    kill x
      | last x == ' ' = kill $ init x
      | otherwise     = x
