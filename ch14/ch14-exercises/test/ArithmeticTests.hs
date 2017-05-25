module ArithmeticTests where

import Arithmetic
import Test.QuickCheck
import Data.List (sort)

halfIdentity :: Double -> Bool
halfIdentity i = ((*2) . half $ i) == i

listOrdered :: (Ord a) => [a] -> Bool
listOrdered as = snd $ foldr go (Nothing, True) as
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

sortedList :: (Arbitrary a, Ord a) => Gen [a]
sortedList = do
  as <- arbitrary
  return (sort as)

sortedIntList :: Gen [Int]
sortedIntList = sortedList

sortedStringList :: Gen [String]
sortedStringList = sortedList

listIntOrdered :: [Int] -> Bool
listIntOrdered = listOrdered

listStringOrdered :: [String] -> Bool
listStringOrdered = listOrdered

main :: IO ()
main = do
  quickCheck halfIdentity
  quickCheck $ forAll sortedIntList listIntOrdered
  quickCheck $ forAll sortedStringList listStringOrdered
