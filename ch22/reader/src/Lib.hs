module Lib
    ( someFunc
    ) where

import Reader

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{-

  compose :: (b -> c)-> (a -> b) -> (a -> c)
  compose f g = \x -> f (g x)

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ \x -> f (ra x)

  which is equivalent to point-free definition

  fmap f (Reader ra) = Reader $ f . ra

-}

{-

-- Applicative f =>
-- f ~ (->) r

pure :: a -> f a
pure :: a -> (r -> a)

(<*>) ::    f (a -> b) ->     f a  ->     f b
(<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)

-}

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f x y = f <$> x <*> y

asks :: (r -> a) -> Reader r a
asks f = Reader f
