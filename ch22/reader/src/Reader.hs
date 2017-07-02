{-# LANGUAGE InstanceSigs #-}

module Reader where

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks = Reader

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  -- when unpacked this type is
  --(<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
  -- so just use a lambda to make the first argument explicit
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

{-
  (>>=) :: Monad m =>      m a -> (a ->      m b) ->      m b
  (>>=) ::            (->) r a -> (a -> (->) r b) -> (->) r b
        ::            (r -> a) ->   (a -> r -> b) -> (r -> b)

  return :: Monad m => a -> m a
  return ::            a -> (->) r a
         ::            a -> (r -> a)
-}

instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb $ ra r) r
