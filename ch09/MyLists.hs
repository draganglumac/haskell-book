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

-- spines and nonstrict evaluation
-- spine - connective structure that ties the collection of values together
-- textually represented by the recursive cons (:) operators
--
-- spine => 1 : 2 : 3 : []
--
-- or =>    1 : (2 : (3 : []))
--
-- or => : <-------|
--      / \        |
--     1   : <-----| this is the "spine"
--        / \      |
--       2   : <---|
--          / \
--         3   []
ljist :: [Char]
ljist = enumFromTo 'a' 'z'

ljist_len :: Int
ljist_len = length ljist

-- 'Normal form'           - expression is fully evaluated.
-- 'Weak head normal form' - expression is only evaluated as far as is necessary
--                         to reach a data constructor.
-- Haskell evaluates values to weak head normal form by default.

-- Pattern matching is strict by default, so pattern matching on cons cells
-- may mean spine strictness is forced if the function doesn't stop recursing
-- the list.

-- `length` is stict in evaluation of the spine but it does not force values
listo :: [Integer]
listo = [1, undefined, 3]
-- If you run `length listo` in GHCi you'll see that it evaluates successfully
-- to 3 despite bottom being the second element in the list.

-- myLength
myLength :: [a] -> Integer
myLength [] = 0
myLength (_ : xs) = 1 + myLength(xs) -- we don't access the value or we ignore it with '_' which only evaluates the spine

-- myMap and myReverse tail-recursive, purely accademic exercise as this post clearly explains
-- https://softwareengineering.stackexchange.com/questions/323270/implementing-map-with-tail-recursion
myMapTail :: (a -> b) -> [a] -> [b]
myMapTail f = go [] where
  go acc [] = myReverseTail acc
  go acc (x : xs) = go ((f x) : acc) xs

myReverseTail :: [a] -> [a]
myReverseTail = go [] where
  go acc [] = acc
  go acc (x : xs) = go (x : acc) xs

-- elegance epitomised
myMap :: (a -> b) -> [a] -> [b]
myMap _ []       = []
myMap f (x : xs) = f x : myMap f xs
