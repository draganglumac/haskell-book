module ApplicativeExamples where

import Data.List (elemIndex)

-- List Applicative

pure' :: a -> [a]
pure' a = [a]

listApply' :: [(a -> b)] -> [a] -> [b]
listApply' _ []         = []
listApply' [f] (x : xs) = (pure (f x)) ++ (listApply' [f] xs)
listApply' (f : fs) ls@(x : xs) = listApply' [f] ls ++ listApply' fs ls

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

i :: Maybe Int
i = elemIndex 3 [1..5]

j :: Maybe Int
j = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> i <*> j

xs = [1, 2, 3] :: [Integer]
ys = [4, 5, 6] :: [Integer]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y' :: Maybe Integer
y' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x' <*> y'
