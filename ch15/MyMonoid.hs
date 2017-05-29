module MyMonoid where

import Test.QuickCheck
import Data.Monoid

-- Monoid laws
leftIdentity :: (Monoid a, Eq a) => a -> Bool
leftIdentity m = mappend mempty m == m

rightIdentity :: (Monoid a, Eq a) => a -> Bool
rightIdentity m = mappend m mempty == m

-- properties to QuickCheck
leftNumIdentity :: Sum Float -> Bool
leftNumIdentity = leftIdentity

rightNumIdentity :: Product Integer -> Bool
rightNumIdentity = rightIdentity

-- go
main :: IO ()
main = do
  quickCheck leftNumIdentity
  quickCheck rightNumIdentity
