module TraversableTests where

import TraversableInstances

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

instance (Arbitrary a, Arbitrary b) => Arbitrary (FstOrSnd a b) where
  arbitrary = frequency [(1, Fst <$> arbitrary), (1, Snd <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (FstOrSnd a b) where (=-=) = eq

testEither :: IO ()
testEither = do
  quickBatch $ traversable (undefined :: [(String, Either String Int, String)])
