module ChapterExercises where

-- Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> r = fmap f r

instance Foldable Identity where
  foldr f z (Identity a) = f a z
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> (f a)
  sequenceA (Identity a) = Identity <$> a

-- Constant
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance Applicative (Constant a) where
  pure = \a -> Constant a
  -- (Constant _) <*> r = r
--
-- instance Foldable (Constant a) where
--   foldr f z (Constant a) = f a z
--   foldMap f (Constant a) = f a
--
-- instance Traversable (Constant a) where
--   traverse f (Constant a) = Constant <$> (f a)
