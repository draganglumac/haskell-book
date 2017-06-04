module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{-
  Applicative functions
  ---------------------

  liftA  :: Applicative f =>           (a -> b) -> f a -> f b
  liftA2 :: Applicative f =>      (a -> b -> c) -> f a -> f b -> f c
  liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d

  Functor vs Applicative
  ----------------------

  class Functor a => Applicative a where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

    fmap  ::    a -> b  -> f a -> f b

  Difference is small, we have an f in front of (a -> b), but the increase in
  power it introduces is profound. For one thing, any Applicative also has a
  Functor and not merely by definition -- you can define a Functor in terms
  of provided Applicative instance.

    fmap f x = pure f <*> x

  How do Monoid-ness and Functor-ness combine for tuple

  instance (Monoid a, Monoid b) => Monoid (a,b) where
    mempty = (mempty, mempty)
    (a, b) `mappend` (a',b') = (a `mappend` a', b `mappend` b')

  instance Monoid a => Applicative ((,) a) where
    pure x = (mempty, x)
    (u,f) <*> (v,x) = (u `mappend` v, f x)

  so for the first element in the tuple combination is done by mappend
  and it needs to be Monoid, but for the second element we combine using
  the function f.

  instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    mappend m Nothing = m
    mappend Nothing m = m
    mappend (Just a) (Just b) = Just (a `mappend` b)

  instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    _ <*> Nothing = Nothing
    Just f <*> Just a = Just (f a)
-}
