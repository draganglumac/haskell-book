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
  quickBatch $ traversable (undefined :: FstOrSnd Int (String, Either String Int, String))
  quickBatch $ traversable (undefined :: Tuple Int (String, Either String Int, String))

-- ChapterExercises
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

-- instance Arbitrary a => Arbitrary (Constant a b) where
--   arbitrary = Constant <$> arbitrary
--
-- instance Eq a => EqProp (Constant a b) where (=-=) = eq

testIdentityAndConstant :: IO ()
testIdentityAndConstant = do
  quickBatch $ traversable (undefined :: Identity (String, Maybe Int, String))
  -- quickBatch $ traversable (undefined :: Constant Int (String, Maybe Int, String))
