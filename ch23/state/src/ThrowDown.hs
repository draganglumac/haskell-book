module ThrowDown where

import System.Random
import Die

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  -- this will produce the same results every
  -- time because it's free of effects
  -- it's fine for this demo
  let s = mkStdGen 0
  let (d1, s1) = randomR (1, 6) s
  let (d2, s2) = randomR (1, 6) s1
  let (d3, _)  = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)
