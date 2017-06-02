module FunctorLaws where

import Test.QuickCheck
import Test.QuickCheck.Function

{- Functor laws

(1) fmap id == id
(2) fmap (p . q) == (fmap p) . (fmap q)

-}

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                  (a -> b)
               -> (b -> c)
               -> f a
               -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

li :: [Int] -> Bool
li x = functorCompose (+1) (*2) x

functorCompose' :: (Eq(f c), Functor f) =>
                   f a
                -> Fun a b
                -> Fun b c
                -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

-- Exercises instances of Functor
-- 1 Identity a
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary =  do
    a <- arbitrary
    return (Identity a)

type IntToString = Fun Int String
type StringToRational = Fun String Rational

-- 2 Pair a
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return (Pair a a)

-- 3 Two a b
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a x) = Two a (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

-- 4 Three a b c
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b x) = Three a b (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

-- 5 Three' a b
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a x y) = Three' a (f x) (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b1 <- arbitrary
    return (Three' a b b1)

-- 6 Four a b c d
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c x) = Four a b c (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
         => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

-- 7 Four' a b
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a1 a2 x) = Four' a a1 a2 (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Four' a a a b)

main :: IO ()
main = do
  quickCheck $ \x -> functorIdentity (x :: [Int])
  quickCheck li
  -- 1 Identity a
  quickCheck $ \x -> functorIdentity (x :: Identity Int)
  quickCheck (functorCompose' :: Identity Int -> IntToString -> StringToRational -> Bool)
  -- 2 Pair a
  quickCheck $ \x -> functorIdentity (x :: Pair Int)
  quickCheck (functorCompose' :: Pair Int -> (Fun Int String) -> (Fun String Rational) -> Bool)
  -- 3 Two a b
  quickCheck $ \x -> functorIdentity (x :: Two String Int)
  quickCheck (functorCompose' :: Two String Int -> (Fun Int String) -> (Fun String Rational) -> Bool)
  -- 4 Three a b c
  quickCheck $ \x -> functorIdentity (x :: Three String Bool Int)
  quickCheck (functorCompose' :: Three String Bool Int -> (Fun Int String) -> (Fun String Rational) -> Bool)
  -- 5 Three' a b
  quickCheck $ \x -> functorIdentity (x :: Three' String Int)
  quickCheck (functorCompose' :: Three' String Int -> (Fun Int String) -> (Fun String Rational) -> Bool)
  -- 6 Four a b c d
  quickCheck $ \x -> functorIdentity (x :: Four String Bool (Maybe Char) Int)
  quickCheck (functorCompose' :: Three String (Maybe Char) Int -> (Fun Int String) -> (Fun String Rational) -> Bool)
  -- 7 Four' a b
  quickCheck $ \x -> functorIdentity (x :: Four' String Int)
  quickCheck (functorCompose' :: Four' String Int -> (Fun Int String) -> (Fun String Rational) -> Bool)
