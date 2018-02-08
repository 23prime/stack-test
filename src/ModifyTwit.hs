module ModifyTwit where

import Data.Char
import System.IO
import System.Environment (getArgs)

run :: IO ()
run = do
  file' <- getArgs
  let file = head file'
  withFile file ReadMode $ \twit -> do
    twits <- hGetContents twit
    writeFile "result.txt" $ seikei twits
    return ()

-- 文字列の整形
seikei :: String -> String
--seikei = unlines . (map concat) . (map killDateUrl) . (map splitSpace) . killRt . lines
seikei = unlines . killEmpty . (map $ concat . killDateUrl . splitSpace) . killRt . lines


-- RT を弾く
killRt :: [String] -> [String]
killRt = filter (\x -> take 2 x /= "RT")

-- Date と URL を消す
killDateUrl :: [String] -> [String]
killDateUrl = map kill
  where
    kill tw
      | maximum (map ord tw) < 10000 = "" -- 英数字のみの文字列を消す
      | otherwise                    = tw

-- 空行を消す
killEmpty :: [String] -> [String]
killEmpty = filter (\x -> x /= "")


-- 文字列をスペースで区切る
splitSpace :: String -> [String]
splitSpace [] = []
splitSpace l = h : splitSpace t
  where
    (h, t) = split (\x -> x == ' ') l


-- 述語 p を最初に満たす地点でリストを前後に分割
split :: (a -> Bool) -> [a] -> ([a], [a])
split _ [] = ([], [])
split p (x : xs)
      | p x       = ([], xs)
      | otherwise = (x : ys, zs)
      where (ys, zs) = split p xs
