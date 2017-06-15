module ChapterExercises where

-- Identity a

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  -- fmap :: (Functor f) => (a -> b) -> f a -> f b
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance Foldable Identity where
  foldr f z (Identity a) = f a z
  -- foldMap :: (Monoid m, Foldable f) => (a -> m) -> f a -> m
  foldMap f (Identity a) = f a

instance Traversable Identity where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse f (Identity a) = fmap Identity (f a)

-- Constant a b

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance (Monoid a) => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant f) <*> (Constant a) = Constant (mappend f a)

instance Foldable (Constant a) where
  -- foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap f (Constant a) = mempty
  foldr f z (Constant a) = z

instance Traversable (Constant a) where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse _ (Constant a) = pure (Constant a)

-- Maybe a

data Try a = None | Some a deriving (Eq, Show)

instance Functor Try where
  fmap _ None = None
  fmap f (Some a) = Some (f a)

instance Applicative Try where
  pure = Some
  None <*> _ = None
  _ <*> None = None
  (Some f) <*> (Some a) = Some (f a)

instance Foldable Try where
  -- foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap _ None = mempty
  foldMap f (Some a) = f a

instance Traversable Try where
  traverse _ None = pure None
  traverse f (Some a) = fmap Some (f a)

-- List a

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  (Cons f fs) <*> as = cat (fmap f as) (fs <*> as) where
    cat Nil as = as
    cat as Nil = as
    cat (Cons x xs) ys = Cons x (cat xs ys)

instance Foldable List where
  -- foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap f Nil = mempty
  foldMap f (Cons a as) = mappend (f a) (foldMap f as)

instance Traversable List where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse f Nil = pure Nil
  traverse f (Cons a as) = Cons <$> f a <*> traverse f as

-- S n a

data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
  -- fmap f (S (g x) x) = S (g . f $ x) (f x)
  fmap f (S n a) = S (fmap f n) (f a)

instance Applicative n => Applicative (S n) where
  pure a = S (pure a) a
  (S f' f) <*> (S g' a) = S (f' <*> g') (f a)

instance Foldable n => Foldable (S n) where
  -- foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap f (S g a) = mappend (foldMap f g) (f a)

instance Traversable n => Traversable (S n) where
  traverse = undefined
