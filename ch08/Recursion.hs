module Recursion where

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

factorialTR :: Integral a => a -> a
factorialTR n = go 1 n
   where go :: Integral a => a -> a -> a
         go acc n
           | n == 0    = acc
           | otherwise = go (acc * n) (n - 1)

fibbonaciTR :: Integral a => a -> a
--fibbonaci 0 = 0
--fibbonaci 1 = 1
--fibbonaci n = fibbonaci (n - 1) + fibbonaci (n - 2)
fibbonaciTR n = go (0, 1) 2 n
  where go :: Integral a => (a, a) -> a -> a -> a
        go (a1, a2) k n
          | n <= 0 = 0
          | n == 1 = 1
          | k < n  = go (a2, a1 + a2) (k + 1) n
          | otherwise = a1 + a2

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)
