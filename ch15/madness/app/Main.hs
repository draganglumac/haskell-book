module Main where

import MadLibs
import System.IO

main :: IO ()
main = do
  putStr "Give me an exclamation: "
  hFlush stdout
  e <- getLine
  putStr "Give me an adverb: "
  hFlush stdout
  adv <- getLine
  putStr "Give me a noun: "
  hFlush stdout
  noun <- getLine
  putStr "Give me an adjective: "
  hFlush stdout
  adj <- getLine
  putStrLn $ madlibbin' e adv noun adj
  putStrLn $ madlibbinBetter e adv noun adj
