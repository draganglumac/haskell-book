module MyMonoid where

import Test.QuickCheck
import Data.Monoid
import Try

-- Monoid laws
leftIdentity :: (Monoid a, Eq a) => a -> Bool
leftIdentity m = mappend mempty m == m

rightIdentity :: (Monoid a, Eq a) => a -> Bool
rightIdentity m = mappend m mempty == m

associativity :: (Monoid a, Eq a) => a -> a -> a -> Bool
associativity x y z = x <> (y <> z) == (x <> y) <> z

concatIsFoldr :: (Monoid a, Eq a) => [a] -> Bool
concatIsFoldr ms = mconcat ms == foldr mappend mempty ms

-- properties to QuickCheck
leftNumIdentity :: Sum Float -> Bool
leftNumIdentity = leftIdentity

rightNumIdentity :: Product Integer -> Bool
rightNumIdentity = rightIdentity

numAssociativity :: Sum Rational -> Sum Rational -> Sum Rational -> Bool
numAssociativity = associativity

numConcatIsFoldr :: [Product Rational] -> Bool
numConcatIsFoldr = concatIsFoldr

-- Any and All boolean monoids for disjunction and conjunction respectivelly
leftIdentityAny :: Any -> Bool
leftIdentityAny = leftIdentity

rightIdentityAll :: All -> Bool
rightIdentityAll = rightIdentity

associativityAny :: Any -> Any -> Any -> Bool
associativityAny = associativity

associativityAll :: All -> All -> All -> Bool
associativityAll = associativity

tryGen :: (Arbitrary a) => Gen (Try a)
tryGen = do
  a <- arbitrary
  return (Some a)

-- our Try Monoid
leftIdentityTry :: Try (Sum Integer) -> Bool
leftIdentityTry = leftIdentity

rightIdentityTry :: Try (Product Rational) -> Bool
rightIdentityTry = rightIdentity

associativityTry :: Try Any -> Try Any -> Try Any -> Bool
associativityTry = associativity

-- go
main :: IO ()
main = do
  quickCheck leftNumIdentity
  quickCheck rightNumIdentity
  quickCheck numAssociativity
  quickCheck numConcatIsFoldr
  quickCheck leftIdentityAny
  quickCheck rightIdentityAll
  quickCheck associativityAny
  quickCheck associativityAll

  -- Maybe - First and Last i.e. whether to return the first or the last
  -- Just value in the series of Maybe values
  print $ show (First (Just 1) <> First (Just 2))
  print $ show (Last (Just 1) <> Last (Just 2))
  print $ show (First Nothing <> First (Just 2))
  print $ show (Last (Just 1) <> Last Nothing)

  quickCheck leftIdentityTry
  quickCheck rightIdentityTry
  verboseCheck associativityTry
