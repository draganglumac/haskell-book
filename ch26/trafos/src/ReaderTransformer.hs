{-# LANGUAGE InstanceSigs #-}

module ReaderTransformer where

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  fmap :: (a -> b)
       -> ReaderT r m a
       -> ReaderT r m b
  fmap fab (ReaderT rma) =
    ReaderT $ fmap fab . rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure a = ReaderT (pure (pure a))

  (<*>) :: ReaderT r m (a -> b)
        -> ReaderT r m a
        -> ReaderT r m b
  (ReaderT fmab) <*> (ReaderT rma) =
    ReaderT $ (<*>) <$> fmab <*> rma

instance (Monad m) => Monad (ReaderT r m) where
  return = pure

  (>>=) :: ReaderT r m a
        -> (a -> ReaderT r m b)
        -> ReaderT r m b
  (ReaderT rma) >>= farmb =
    ReaderT $ \r -> do
      a <- rma r
      runReaderT (farmb a) r
