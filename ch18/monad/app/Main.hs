module Main where

import Playground

sequenceActions :: (IO (), IO ())
sequenceActions = (first, second) where
  first = putStrLn "First"
  second = putStrLn "Second"

main :: IO ()
main = do
  honkIfYouLikeBind
  fst sequenceActions
  snd sequenceActions
  (fst sequenceActions) >> (snd sequenceActions)
  (fst sequenceActions) *> (snd sequenceActions)
  putStrLn "badoomBadoom"
  badoomBadoom
