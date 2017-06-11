module FoldableInstances where

import Data.Foldable
import Data.Monoid

-- Identity
newtype Identity a = Identity a deriving (Eq, Show)

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

-- Try a
data Try a = None | Some a deriving (Eq, Show)

instance Foldable Try where
  foldr f z None = z
  foldr f z (Some x) = f x z
  -- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
  foldMap f None = mempty
  foldMap f (Some x) = f x

-- implement library functions
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (Any . (==x))

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr maybeMin Nothing where
             maybeMin x Nothing = Just x
             maybeMin x acc@(Just y) = if x < y then Just x else acc

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr maybeMax Nothing where
             maybeMax x Nothing = Just x
             maybeMax x acc@(Just y) = if x > y then Just x else acc

length' :: (Foldable t) => t a -> Int
length' = foldr count 0 where
            count x acc = 1 + acc

toList' :: Foldable t => t a -> [a]
toList' = foldr (:) []

null' :: (Foldable t, Eq a) => t a -> Bool
null' x = [] == take 1 (toList' x)

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-- foldMap in terms of foldr
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty
