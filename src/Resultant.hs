module Resultant where

import Numeric.LinearAlgebra

type Poly = [R]
  -- 多項式はこれで表す
  -- 主係数から定数までを，次数の大きい項から順に全て列挙
  -- 例 : x^2 - 2 -> [1, 0, (-2)]

deg :: Poly -> Int -- 次数計算
deg f = length f - 1


mj :: Poly -> Poly -> Int -> Matrix R -- M_j (f, g) の計算
mj f g j
  | j < 0 || k <= 0 = undefined
  | otherwise       = (k >< k) $ concat -- 行ごとの配列になっているものをくっつける
                      $  ((makeMsRow f k) <$> [0..(n - j - 1)]) -- 上側
                      ++ ((makeMsRow g k) <$> [0..(m - j - 1)]) -- 下側
  where
    (m, n) = (deg f, deg g)
    k = m + n - 2 * j

syl :: Poly -> Poly -> Matrix R
syl f g = mj f g 0 -- シルベスター行列


psc :: Poly -> Poly -> Int -> R -- PSC_j (f, g) の計算
psc f g = det . mj f g


res :: Poly -> Poly -> R
res f g = det $ syl f g -- シルベスター行列の PSC

degGcd :: Poly -> Poly -> Int
degGcd f g = head $ filter (\j -> psc f g j /= 0) [0..] -- PSC が最初に 0 じゃなくなる j


makeMsRow :: Poly -> Int -> Int -> Poly          -- M_j の各行を生成
makeMsRow f k = flip adjustLength k . add0Head f -- 右に n ずらしてから長さ k の配列を作る


add0Head :: Poly -> Int -> Poly
add0Head a 0 = a
add0Head a n    -- n 個の 0 を先頭に加える
  | n < 0     = undefined
  | otherwise = 0 : add0Head a (n - 1)


adjustLength :: Poly -> Int -> Poly
adjustLength a n -- 配列の長さを n にする
  | dif == 0  = a
  | dif <  0  = take n a             -- 長い分を削る
  | otherwise = a ++ replicate dif 0 -- 短い分 0 を後ろに加える
  where
    dif = n - length a

mat :: Matrix R
mat = (6 >< 6) [ 3, 1, 2, 5, 2, 2
               , 5, 2, 3, 3, 1, 2
               , 2, 0, 1, 5, 3, 4
               , 4, 3, 5, 2, 1, 2
               , 1, 2, 3, 5, 6, 4
               , 6, 4, 1, 4, 3, 3
               ]
