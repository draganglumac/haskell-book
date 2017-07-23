module FizzBuzzDL where

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.DList as DL

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod`  3 == 0 = "Fizz"
           | n `mod`  5 == 0 = "Buzz"
           | otherwise       = show n

fizzBuzzList :: [Integer] -> DL.DList String
fizzBuzzList list =
  execState (mapM_ addResult list) DL.empty

fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo start end =
  execState (mapM_ addResultList (enumFromThenTo end (end - 1) start)) []

addResultList :: Integer -> State [String] ()
addResultList n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  -- snoc appends to the end, unlike cons which appends to the front
  put (DL.snoc xs result)

main :: IO ()
main =
  -- mapM_ putStrLn $ fizzBuzzList [1..100]
  mapM_ putStrLn $ fizzBuzzFromTo 1 100
