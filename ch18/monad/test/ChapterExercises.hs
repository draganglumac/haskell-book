module ChapterExercises where

import Prelude hiding (Left, Right)

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure a = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  NopeDotJpg >>= f = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where (=-=) = eq

nopeTrigger :: Nope (Int, Int, Int)
nopeTrigger = undefined

data PhbtEither b a = Left a | Right b deriving (Eq, Show)

instance Functor (PhbtEither b) where
  fmap _ (Right b) = Right b
  fmap f (Left a) = Left (f a)

instance Applicative (PhbtEither b) where
  pure = Left
  _ <*> (Right b) = Right b
  (Right f) <*> _ = Right f
  (Left f) <*> (Left a) = Left (f a)

instance Monad (PhbtEither b) where
  return = pure
  (Right b) >>= _ = Right b
  (Left a) >>= f = f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhbtEither b a) where
  arbitrary = frequency [(1, Left <$> arbitrary), (1, Right <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (PhbtEither b a) where (=-=) = eq

phbtEitherTrigger :: PhbtEither Int (Int, String, Int)
phbtEitherTrigger = undefined

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> i = fmap f i

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

identityTrigger :: Identity (Int, String, Int)
identityTrigger = undefined

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  (Cons f fs) <*> (Cons a as) = undefined

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  (Cons a as) >>= f = undefined

verifyMonadLawsForExercises :: IO ()
verifyMonadLawsForExercises = do
  -- Nope a
  putStrLn "\nNope a"
  putStrLn "------"
  quickBatch $ functor nopeTrigger
  quickBatch $ applicative nopeTrigger
  quickBatch $ monad nopeTrigger
  -- PhbtEither b a
  putStrLn "\nPhbtEither b a"
  putStrLn "--------------"
  quickBatch $ functor phbtEitherTrigger
  quickBatch $ applicative phbtEitherTrigger
  quickBatch $ monad phbtEitherTrigger
  -- Identity a
  putStrLn "\nIdentity a"
  putStrLn "----------"
  quickBatch $ functor identityTrigger
  quickBatch $ applicative identityTrigger
  quickBatch $ monad identityTrigger
