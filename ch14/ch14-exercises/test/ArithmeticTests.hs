module ArithmeticTests where

import Arithmetic
import Test.QuickCheck

halfIdentity :: Double -> Bool
halfIdentity i = ((*2) . half $ i) == i

main :: IO ()
main = do
  quickCheck halfIdentity
