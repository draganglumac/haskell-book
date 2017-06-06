module Listos where

import Data.Monoid

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil as = as
  mappend as Nil = as
  mappend (Cons a as) bs = Cons a $ mappend as bs

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  -- pure
  pure a = Cons a Nil
  -- apply
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f fs) as = fmap f as <> (fs <*> as)

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' n (Cons a as) = if n > 0
                      then Cons a (take' (n-1) as)
                      else Cons a Nil

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ f <$> xs

instance Applicative ZipList' where
  pure a = ZipList' (pure a)
  ZipList' fs <*> ZipList' as = ZipList' (fs <*> as)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as -- or f <$> as if you prefer
