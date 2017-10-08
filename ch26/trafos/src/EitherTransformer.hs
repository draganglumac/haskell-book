{-# LANGUAGE InstanceSigs #-}


module EitherTransformer where
newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap :: (a -> b)
       -> EitherT e m a
       -> EitherT e m b
  fmap f (EitherT mea) =
    EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure a = EitherT $ pure (pure a)

  (<*>) :: EitherT e m (a -> b)
        -> EitherT e m a
        -> EitherT e m b
  (EitherT mfab) <*> (EitherT ma) =
    EitherT $ fmap (<*>) mfab <*> ma

instance Monad m => Monad (EitherT e m) where
  return = pure

  (>>=) :: EitherT e m a
        -> (a -> EitherT e m b)
        -> EitherT e m b
  (EitherT mea) >>= f =
    EitherT $ do
      ea <- mea
      case ea of
        Left e -> return (Left e)
        Right a -> runEitherT $ f a

swapEitherT :: (Functor m)
           => EitherT e m a
           -> EitherT a m e
swapEitherT (EitherT mea) =
  EitherT $ fmap swap mea where
    swap (Left a) = Right a
    swap (Right e) = Left e

eitherT :: Monad m
        => (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT f g (EitherT mab) = do
  ab <- mab
  case ab of
    Left a -> f a
    Right b -> g b
