{-# LANGUAGE DeriveGeneric #-}

module SemigroupTests where

import Data.Semigroup
import Test.QuickCheck
import GHC.Generics

assoc :: (Semigroup a, Eq a) => a -> a -> a -> Bool
assoc x y z = x <> (y <> z) == (x <> y) <> z

-- data Trivial
data Trivial = Trivial deriving (Eq, Show)
instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

-- newtype Identity a
newtype Identity a = Identity a deriving (Eq, Show)
instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity b = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

-- data Two a b
data Two a b = Two a b deriving (Eq, Show)
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two x1 y1 <> Two x2 y2 = Two (x1 <> x2) (y1 <> y2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

-- newtype Combine a b
newtype Combine a b = Combine { unCombine :: a -> b } deriving (Generic)

instance (Semigroup b) => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine $ \x -> f x <> g x

main :: IO ()
main = do
  quickCheck (assoc :: Trivial
                    -> Trivial
                    -> Trivial
                    -> Bool)
  quickCheck (assoc :: Identity (Sum Int)
                    -> Identity (Sum Int)
                    -> Identity (Sum Int)
                    -> Bool)
  quickCheck (assoc :: Two (Sum Int) (Product Int)
                    -> Two (Sum Int) (Product Int)
                    -> Two (Sum Int) (Product Int)
                    -> Bool)
  -- might need lifting so probably best attempted at a later time
