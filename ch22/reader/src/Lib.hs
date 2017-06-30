module Lib
    ( someFunc
    ) where

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
