module Identity where

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity fab) (Identity a) = Identity (fab a)

-- Constant e

newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant e) = Constant e

instance Monoid a => Applicative (Constant a) where
  pure a = Constant mempty
  (<*>) (Constant a1) (Constant a2) = Constant (mappend a1 a2)
