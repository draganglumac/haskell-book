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
