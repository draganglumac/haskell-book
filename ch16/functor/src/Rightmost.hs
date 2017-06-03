module Rightmost where

data Two' a b = Two' a b deriving (Eq, Show)

data Or a b = First a | Second b deriving (Eq, Show)

{-

The following two won't work because Functor is of kind * -> *
and Two and Or are both of kind * -> * -> *

instance Functor Two where
  fmap = undefined

instance Functor Or where
  fmap = undefined

We need to include the innermost type in the instance definition.
-}

instance Functor (Two' a) where
  fmap f (Two' a b) = Two' a (f b)

instance Functor (Or a) where
  fmap _ (First a)  = First a -- we have applied out the first argument a, so now it's part of the f
  fmap f (Second b) = Second (f b)

-- Wrap f a
data Wrap f a = Wrap (f a) deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

{- works for any Functor

*Main Lib Rightmost> fmap (+1) (Wrap (Just 1))
Wrap (Just 2)
*Main Lib Rightmost> fmap (+1) (Wrap [1, 2 ,3])
Wrap [2,3,4]

-}
