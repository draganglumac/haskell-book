module TraversableTests where

import TraversableInstances
import ChapterExercises

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

instance (Arbitrary a, Arbitrary b) => Arbitrary (FstOrSnd a b) where
  arbitrary = frequency [(1, Fst <$> arbitrary), (1, Snd <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (FstOrSnd a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Tuple a b)

instance (Eq a, Eq b) => EqProp (Tuple a b) where (=-=) = eq

testEitherAndTuple :: IO ()
testEitherAndTuple = do
  putStrLn "FstOrSnd a b"
  quickBatch $ traversable (undefined :: FstOrSnd Int (String, Either String Int, String))
  putStrLn "Tuple a b"
  quickBatch $ traversable (undefined :: Tuple Int (String, Either String Int, String))

-- ChapterExercises
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where (=-=) = eq

testIdentityAndConstant :: IO ()
testIdentityAndConstant = do
  putStrLn "Identity a"
  quickBatch $ traversable (undefined :: Identity (String, Maybe Int, String))
  putStrLn "Constant a b"
  quickBatch $ traversable (undefined :: Constant Int (String, Maybe Int, String))

instance Arbitrary a => Arbitrary (Try a) where
  arbitrary = frequency [(1, return None), (1, Some <$> arbitrary)]

instance Eq a => EqProp (Try a) where (=-=) = eq

listToList :: [a] -> List a
listToList = foldr Cons Nil

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fmap listToList (listOf arbitrary)

instance CoArbitrary a => CoArbitrary (List a) where
  coarbitrary Nil = variant 0
  coarbitrary (Cons x xs) = variant 1 . coarbitrary (x, xs)

instance Eq a => EqProp (List a) where (=-=) = eq

testTryList :: IO ()
testTryList = do
  putStrLn "Try a"
  quickBatch $ traversable (undefined :: Try (String, Maybe Int, String))
  putStrLn "List a"
  quickBatch $ traversable (undefined :: List (String, Maybe Int, String))
