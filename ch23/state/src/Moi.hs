{-# LANGUAGE InstanceSigs #-}

module Moi where

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  -- courtesy of https://lukleh.github.io/haskell-book-exercises/#_23_6_write_state_for_yourself
  fmap f (Moi g) = Moi $ \s -> let (a, b) = g s in (f a, b)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  -- courtesy of https://lukleh.github.io/haskell-book-exercises/#_23_6_write_state_for_yourself
  (Moi f) <*> (Moi g) = Moi $ \s -> let fab = fst $ f s
                                        (a, b) = g s
                                    in (fab a, b)

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  -- courtesy of https://lukleh.github.io/haskell-book-exercises/#_23_6_write_state_for_yourself
  (Moi f) >>= g = Moi $ \s -> let a = fst $ f s
                                  ms = runMoi $ g a
                              in ms s
