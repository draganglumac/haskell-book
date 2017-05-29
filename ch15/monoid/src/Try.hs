module Try where

import Test.QuickCheck

data Try a = None | Some a deriving (Eq, Show)

instance Monoid a => Monoid (Try a) where
  mempty = None
  mappend a1 a2 = case (a1, a2) of
                  (_, None) -> a1
                  (None, _) -> a2
                  (Some x, Some y) -> Some (mappend x y)

instance (Arbitrary a) => Arbitrary (Try a) where
  arbitrary = do
    a <- arbitrary
    oneof [return None, return (Some a)]
