module MyLists where

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x : xs) = Just xs

eft :: (Eq a, Ord a, Enum a) => a -> a -> [a]
eft start end
  | start >  end = []
  | start == end = [start]
  | otherwise    = go [] start end
      where go as s e
              | s == e    = s : as
              | otherwise = go (e : as) s (pred e)

eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft

-- exercises
-- list of words from a sentence
wordz :: String -> [String]
wordz s
  | s == ""   = []
  | otherwise =
      (takeWhile (/= ' ') s)
      : (wordz (dropWhile (== ' ') (dropWhile (/= ' ') s)))


-- comprehensions
squaredCubes :: Integral a => [(a, a)]
squaredCubes = [(x^2, y^3) | x <- [1..5], y <- [1..5], x^2 < 50, y^3 < 50]
