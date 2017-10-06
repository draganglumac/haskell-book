{-# LANGUAGE InstanceSigs #-}
module MonadTransformers where

import Control.Monad

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT ma) = IdentityT $ fmap f ma

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (IdentityT mfab) <*> (IdentityT ma) = IdentityT (mfab <*> ma)

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  (>>=) :: IdentityT m a
        -> (a -> IdentityT m b)
        -> IdentityT m b
--  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f
--               [1]                    [2]
-- 1. Type of this bind is :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
-- 2. Type of this bind is :: m a -> (a -> m b) -> m b so we need to runIdentityT
  (IdentityT ma) >>= f =
    -- [1] try >>= blindly
    --   let aimb :: a (anything basically)
    --   in aimb = ma >>= f
    -- barfs with Couldn't match type ‘m’ with ‘IdentityT m’ so no help
    -- [2] try fmap instead
    --   let aimb :: a
    --   in aimb = fmap f ma
    -- barfs with Couldn't match expected type ‘a1’ with actual type ‘m (IdentityT m b)’
    -- so we need to remove IdentityT between two m's so we can join them
    -- [3] must runIdentityT to remove the hurdle
    --   let aimb :: a
    --   in aimb = fmap runIdentityT (fmap f ma)
    -- barfs with Couldn't match expected type ‘a1’ with actual type ‘m (m b)’
    -- so now we can join
    -- [4]
    -- let aimb = join (fmap runIdentityT (fmap f ma))
    -- this succeeds but we can refactor
    -- use Functor law fmap (f . g) = fmap f . fmap g
    -- [5]
    -- let aimb = join (fmap (runIdentityT . f) ma)
    -- this succeeds but we can still refactor
    -- use x >>= f = join (fmap f x)
    IdentityT $ ma >>= runIdentityT . f
    -- transformers library implements it a bit differently but it's equivalent
    --   m >>= k = runIdentityT . k =<< runIdentityT m
