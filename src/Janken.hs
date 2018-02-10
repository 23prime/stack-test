module Janken where

import System.Random

data Move = Gu | Choki | Pa
  deriving (Show, Eq)

type Round = (Move, Move)
type Strategy1 = [Move] -> Move

instance Ord Move where
  compare Gu Choki = LT
  compare Choki Pa = LT
  compare Pa Gu    = LT
  compare x y
    | x == y    = EQ
    | otherwise = GT

score :: Round -> (Int, Int)
score (x, y)
  | x < y     = (1, 0)
  | y < x     = (0, 1)
  | otherwise = (0, 0)

rounds1 :: (Strategy1, Strategy1) -> [Round]
rounds1 (p1, p2) = map head $ tail $ iterate (extend (p1, p2)) []

extend :: (Strategy1, Strategy1) -> [Round] -> [Round]
extend (p1, p2) rs = (p1 (map snd rs), p2 (map fst rs)) : rs

match :: Int -> (Strategy1, Strategy1) -> (Int, Int)
match n = total . map score . take n . rounds1
  where
    total rs = (sum (map fst rs), sum (map snd rs))

rand :: Int -> Int
rand n = fst $ randomR (0, n - 1) $ mkStdGen n

pick :: (Int, Int ,Int) -> Move
pick (g, c, p)
  | m < g     = Pa
  | m < g + c = Gu
  | otherwise = Choki
  where
    m = rand $ g + c + p

count :: Move -> (Int, Int, Int) -> (Int, Int, Int)
count Gu    (g, c, p) = (g + 1, c, p)
count Choki (g, c, p) = (g, c + 1, p)
count Pa    (g, c, p) = (g, c, p + 1)

copy1 :: Strategy1
copy1 ms
  | null ms   = Gu
  | otherwise = head ms

smart1 :: Strategy1
smart1 ms
  | null ms   = Gu
  | otherwise = pick (foldr count (0, 0, 0) ms)
