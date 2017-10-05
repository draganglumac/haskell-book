module Bifunctor where

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b)
        -> (c -> d)
        -> p a c
        -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (c -> d) -> p a c -> p a d
  second = bimap id

-- Bifunctor instances

data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux a c) = Deux (f a) (g c)

newtype Const a b = Const a

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)

newtype SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadriceps a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadriceps a b c d) = Quadriceps a b (f c) (g d)

instance Bifunctor Either where
  bimap f g (Left a) = Left (f a)
  bimap f g (Right b) = Right (g b)
