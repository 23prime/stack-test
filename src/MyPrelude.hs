{-# LANGUAGE DataKinds
           , KindSignatures
           , ScopedTypeVariables
           , ExistentialQuantification
           , DeriveFunctor
           , StandaloneDeriving
           , GADTs
           , MonomorphismRestriction
#-}

module MyPrelude where

import ModifyTwit
import IOAction
--import Practice
import Algebra hiding (fib)
import Random
import Binary
import Polynomial

import Control.DeepSeq
import Control.Monad
--import Control.Monad.Ref
import Control.Monad.ST
import Control.Monad.State
import Data.Array.ST
import Data.STRef

import System.IO
import System.Process

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs : xss) = let yss = cp xss
                in  [x : ys | x <- xs, ys <- yss]


-- State Monad のサンプル
-- ラベル付き端点を持つ二分木
data BinTree a = Leaf a
               | Fork (BinTree a) (BinTree a)
  deriving (Show, Eq, Ord)

labels :: BinTree a -> [a]
labels (Leaf x)   = [x]
labels (Fork u v) = labels u ++ labels v


build :: [a] -> BinTree a
build xs = evalState (build' $ length xs) xs

build' :: Int -> State [a] (BinTree a)
build' 1 = do
  x : xs <- get
  put xs
  return (Leaf x)
build' n = do
  let m = n `div` 2
  u <- build' m
  v <- build' (n - m)
  return (Fork u v)


-- ST Monad
fib :: Int -> Integer
fib n = fst (fib' n)

fib' 0 = (0, 1)
fib' n = let (a, b) = fib' (n - 1)
         in  (b, a + b)

fibST :: Int -> ST s Integer
fibST n = do
  a <- newSTRef 0 -- newSTRef :: a -> ST s (STRef s a)
  b <- newSTRef 1
  repeatFor n
    (do
      x <- readSTRef a -- readSTRef :: STRef s a -> ST s a
      y <- readSTRef b
      writeSTRef a y -- writeSTRef :: STRef s a -> a -> ST s ()
      writeSTRef b $! (x + y)
    )
  readSTRef a

repeatFor :: Monad m => Int -> m a -> m ()
repeatFor n = foldr (>>) done . replicate n

done :: Monad m => m ()
done = do
  return ()


-- 可変配列
-- STArray s i e
-- s : スレッド名
-- i : インデックスの型 <= Ix i
-- e : 要素の型

-- newListArray :: (Ix i, MArray a e m) => (i, i) -> [e] -> m (a i e)
-- getElems     :: (Ix i, MArray a e m) => a i e -> m [e]
-- readArray    :: (Ix i, MArray a e m) => a i e -> i -> m e
-- writeArray   :: (Ix i, MArray a e m) => a i e -> i -> e -> m ()

qsort :: Ord a => [a] -> [a]
qsort xs = runST $ do
    xa <- newListArray (0, n - 1) xs
    qsortST xa (0, n)
    getElems xa
  where
    n = length xs

-- 区間 (a, b) のリスト == [a .. b - 1] をソート
qsortST :: Ord a => STArray s Int a -> (Int, Int) -> ST s ()
qsortST xa (a, b)
  | a == b    = return ()
  | otherwise = do
      m <- partition xa (a, b)
      qsortST xa (a, m)
      qsortST xa (m + 1, b)

partition :: Ord a => STArray s Int a -> (Int, Int) -> ST s Int
partition xa (a, b) = do
  x <- readArray xa a
  let loop (j, k)
        = if j == k
          then do
            swap xa a (k - 1)
            return (k - 1)
          else do
            y <- readArray xa j
            if y < x
              then do
                loop (j + k, k)
              else do
                swap xa j (k - 1)
                loop (j, (k - 1))
  loop (a + 1, b)

swap :: STArray s Int a -> Int -> Int -> ST s ()
swap xa i j = do
  v <- readArray xa i
  w <- readArray xa j
  writeArray xa i w
  writeArray xa j v
