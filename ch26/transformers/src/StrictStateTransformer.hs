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
      let mf = smf s in
        let ma = sma s in
        do (f, _) <- mf
           (a, s') <- ma
           return (f a, s')
