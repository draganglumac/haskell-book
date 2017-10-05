module Main where

import Lib
import Compose

import Data.Monoid

main :: IO ()
main = do
  c <- return (Compose [Just 1, Nothing])
  print (head $ getCompose $ fmap (+1) c)
  print $ foldMap Sum (Compose [Just 1, Just 2])
  print $ foldMap Product (Compose [Just 1, Just 2])
