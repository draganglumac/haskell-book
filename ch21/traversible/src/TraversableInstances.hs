module TraversableInstances where

-- Either

data FstOrSnd a b = Fst a | Snd b deriving (Eq, Ord, Show)

instance Functor (FstOrSnd a) where
  fmap _ (Fst a) = Fst a
  fmap f (Snd b) = Snd $ f b

instance Applicative (FstOrSnd a) where
  pure = Snd
  Fst a <*> _ = Fst a
  Snd f <*> s = fmap f s

instance Foldable (FstOrSnd a) where
  foldMap _ (Fst _) = mempty
  foldMap f (Snd b) = f b

  foldr _ z (Fst _) = z
  foldr f z (Snd b) = f b z

instance Traversable (FstOrSnd a) where
  -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse _ (Fst a) = pure (Fst a)
  traverse f (Snd b) = Snd <$> f b

-- (,) a b

data Tuple a b = Tuple a b deriving (Eq, Show)

instance Functor (Tuple a) where
  fmap f (Tuple a b) = Tuple a (f b)

instance Monoid a => Applicative (Tuple a) where
  pure b = Tuple mempty b
  (Tuple u f) <*> (Tuple v b) = Tuple (mappend u v) (f b)

instance Foldable (Tuple a) where
  foldMap f (Tuple _ b) = f b
  foldr f z (Tuple _ b) = f b z

instance Traversable (Tuple a) where
  traverse f (Tuple a b) = Tuple a <$> f b

-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
-- foldr :: Foldable t = > (a -> b -> b) -> b -> t a -> b
