module Gauntlet where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random
import Die

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

-- less verbosely
rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie' rollDie' rollDie'

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie'

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= 20 = count
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1) nextGen

rollsToGetN :: Int -> StdGen -> (Int, [Die])
rollsToGetN n g = go 0 (0, []) g
  where go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
        go sum (count, acc) gen
          | sum >= n = (count, reverse acc)
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1, (intToDie die) : acc) nextGen

main :: IO ()
main = do
  print $ "evalState rollDieThreeTimes' (mkStdGen 0) = " ++ show (evalState rollDieThreeTimes' (mkStdGen 0))
  print $ "evalState rollDieThreeTimes' (mkStdGen 1) = " ++ show (evalState rollDieThreeTimes' (mkStdGen 1))
  print $ "evalState (nDie 5) (mkStdGen 0) = " ++ show (evalState (nDie 5) (mkStdGen 0))
  print $ "evalState (nDie 5) (mkStdGen 1) = " ++ show (evalState (nDie 5) (mkStdGen 1))
  print $ "rollsToGetTwenty (mkStdGen 0) = " ++ show (rollsToGetTwenty (mkStdGen 0))
  print $ "rollsToGetN 20 (mkStdGen 0) = " ++ show (rollsToGetN 20 (mkStdGen 0))
