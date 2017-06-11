module ChapterExercises where

import Data.Foldable
import Data.Monoid

newtype Constant a b = Constant a deriving (Eq, Show)

instance Foldable (Constant a) where
  -- f takes a value of type b (from Constant a b) as the first argument
  foldr f z (Constant a) = z

data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
  foldr f z (Two a b) = f b z

data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
  foldr f z (Three a b c) = f c z

data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
  foldr f z (Three' a b1 b2) = f b2 (f b1 z)

data Four' a b = Four' a b b b deriving (Eq, Show)

instance Foldable (Four' a) where
  foldr f z (Four' a b1 b2 b3) = f b3 (f b2 (f b1 z))

type Body = String
type Dim = Integer

type Volume = Four' Body Dim

-- filter using foldMap
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)

testFilterF :: IO ()
testFilterF = do
  putStrLn $ "`filterF (> 3) [1, 2, 3, 4, 5] :: [Int]` = "
             ++ show (filterF (> 3) [1, 2, 3, 4, 5] :: [Int])
  putStrLn $ "`filterF even [1..10] :: [Int]` = "
             ++ show (filterF even [1..10] :: [Int])
  putStrLn $ "`filterF (> 'a') \"Barnaby\" :: String` = "
             ++ show (filterF (> 'a') "Barnaby" :: String)
  putStrLn $ "`filterF (> 3) [1..10] :: Sum Int` = "
             ++ show (filterF (> 3) [1..10] :: Sum Int)
