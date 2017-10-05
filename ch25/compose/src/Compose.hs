{-# LANGUAGE InstanceSigs #-}
module Compose where

import Control.Applicative

newtype Identity a = Identity { runIdentity :: a }

newtype Compose f g a = Compose { getCompose :: f (g a) }
                          deriving (Eq, Show)

instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  -- make me pure
  pure :: a -> Identity a
  pure = Identity
  -- the apply (<*>) operator
  (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  (Identity f) <*> (Identity a) = Identity (f a)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap :: (a -> b) -> Compose f g a -> Compose f g b
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  -- make me pure
  pure :: a -> Compose f g a
  pure = Compose <$> (pure . pure)
  -- the apply (<*>) operator
  -- from https://wiki.haskell.org/Applicative_functor
  --   fgh :: f (g (a -> b))
  --   fga :: f (g a)
  --   liftA2 (<*>) fgh fga :: f (g b)
  -- That is, liftA2 (<*>) is essentially the definition for <*> for
  -- the composition of the functors f and g.
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose fa) <*> (Compose a) = Compose $ liftA2 (<*>) fa a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: (Monoid m) => (a -> m) -> Compose f g a -> m
  -- reduced to point-free style from haskell-mod suggestions
  --   foldMap (\x -> foldMap (\y -> am y) x) a
  foldMap am (Compose a) = foldMap (foldMap am) a

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: (Applicative f1) => (a -> f1 b) -> Compose f g a -> f1 (Compose f g b)
  -- reduced to point-free style from haskell-mod suggestions
  --   Compose <$> traverse (\f -> traverse (\g -> af1b g) f) fga
  traverse af1b (Compose fga) = Compose <$> traverse (traverse af1b) fga
