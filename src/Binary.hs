module Binary where

data Binary = B Integer

instance Show Binary where
  show (B m)
    | null $ filter (flip elem $ show m) "23456789" = show m
    | otherwise                                     = undefined

instance Eq Binary where
  (B m) == (B n) = m == n

instance Ord Binary where
  compare (B m) (B n) = compare m n

instance Num Binary where
  (B m) + (B n) = toBinary $ fromBinary (B m) + fromBinary (B n) -- この辺すごい横着
  (B m) - (B n) = toBinary $ fromBinary (B m) - fromBinary (B n)
  (B m) * (B n) = toBinary $ fromBinary (B m) * fromBinary (B n)
  negate (B m)  = undefined
  abs (B m)     = B m
  signum _      = 1
  fromInteger m = toBinary m


fromBinary :: Binary -> Integer
fromBinary b = snd $ loop (show b) (0, 0)
  where
    loop [] (n, x) = (n, x)
    loop b (n, x)
      | last b == '0' = loop (init b) (n + 1, x)
      | last b == '1' = loop (init b) (n + 1, x + 2 ^ n)
      | otherwise     = undefined

toBinary :: Integer -> Binary
toBinary n = B (read $ int2BinaryStr n :: Integer)

int2BinaryStr 0 = "0"
int2BinaryStr n = concat $ map (show . snd) $ init $ loop [(n, 0)]
  where
    loop xs
      | fst (head xs) == 0 = xs
      | otherwise          = loop $ ((fst $ head xs) `divMod` 2) : xs

-- 2進桁数
digit2 = length . int2BinaryStr

-- スマッシュ関数とかいうらしい
x # y = 2 ^ (digit2 x * digit2 y)
