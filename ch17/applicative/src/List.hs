module List where

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil as = as
  mappend as Nil = as
  mappend (Cons a as) bs = Cons a $ mappend as bs

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  -- pure
  pure a = Cons a Nil
  -- apply
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f fs) as = fmap f as `mappend` (fs <*> as)

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' n (Cons a as) = if n > 0
                      then Cons a (take' (n-1) as)
                      else Cons a Nil
