module Main where

import qualified DogsRule
import Hello

main :: IO ()
main = do
  sayHello
  DogsRule.dogs
