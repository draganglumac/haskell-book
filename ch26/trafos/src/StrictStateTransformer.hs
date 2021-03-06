{-# LANGUAGE InstanceSigs #-}

module StrictStateTransformer where

import Control.Arrow (first)

newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b)
       -> StateT s m a
       -> StateT s m b
  fmap f (StateT sma) =
    -- StateT $ fmap (\(a, s) -> (f a, s)) . sma
    StateT $ fmap (first f) . sma

instance Monad m => Applicative (StateT s m) where
  pure x = StateT (\s -> pure (x, s))

  (<*>) :: StateT s m (a -> b)
        -> StateT s m a
        -> StateT s m b
  (StateT smf) <*> (StateT sma) =
    StateT $ \s ->
      do (f, s1) <- smf s
         (a, s2) <- sma s1
         pure (f a, s2)

instance Monad m => Monad (StateT s m) where
  return = pure

  (>>=) :: StateT s m a
        -> (a -> StateT s m b)
        -> StateT s m b
  (StateT sma) >>= f =
    StateT $ \s ->
      do (a, s1) <- sma s
         runStateT (f a) s1
