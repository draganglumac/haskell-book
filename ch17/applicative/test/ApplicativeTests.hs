module ApplicativeTests where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools)
                        , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

instance EqProp Bull where (=-=) = eq

badMonoid :: IO ()
badMonoid = quickBatch (monoid Twoo) -- the value Twoo is not used, it's there just to the checkers the type we wont for Monoid

-- Applicative tests via `applicative` TestBatch in checkers
applicativeList :: IO ()
-- again the value is not used, it's just there for the type
applicativeList = quickBatch $ applicative [("b", "w", 1 :: Int)]

applicativeListBottom :: IO ()
-- which is why we can use bottom here to specify type
applicativeListBottom = quickBatch $ applicative (undefined :: [(String, String, Int)])
