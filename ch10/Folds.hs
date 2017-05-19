module Folds where

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f as = myOr (map f as)

myElem1 :: Eq a => a -> [a] -> Bool
myElem1 a as =  foldr (\x acc -> x == a || acc) False as

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 a as = myAny (== a) as

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myReverseR :: [a] -> [a]
myReverseR = foldr (\x acc -> acc ++ [x]) []

mapViaFold :: (a -> b) -> [a] -> [b]
mapViaFold f = foldr (\x acc -> f x : acc) []

filterViaFold :: (a -> Bool) -> [a] -> [a]
filterViaFold f = foldr (\a acc -> if f a then a : acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (\a acc -> a ++ acc) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f as = squish (map f as)
