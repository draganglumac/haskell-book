module Main where

import ApplicativeExamples
import Identity
import Data.Monoid
import Control.Applicative
import Data.Char

main :: IO ()
main = do
  putStrLn $ "listApply' (fmap (,) [1, 2]) [3, 4]      = " ++
    show (listApply' (fmap (,) [1, 2]) [3, 4])
  putStrLn $ "liftA2 (,) [1, 2] [3, 4]                 = " ++
    show (liftA2 (,) [1, 2] [3, 4])
  putStrLn $ "max <$> [1, 2] <*> [3, 4]                = " ++
    show (max <$> [1, 2] <*> [3, 4])
  putStrLn $ "liftA2 (+) [1, 2] [3, 4]                 = " ++
    show (liftA2 (+) [1, 2] [3, 4])
  putStrLn $ "lookup 3 [(3, \"hello\")]                  = " ++
    show (lookup 3 [(3, "hello")])
  putStrLn $ "fmap length $ lookup 3 [(3, \"hello\")]    = " ++
    show (fmap length $ lookup 3 [(3, "hello")])
  let upCase str = map toUpper str in
    putStrLn $ "fmap upCase $ lookup 3 [(3, \"hello\")]    = " ++
      show (fmap upCase $ lookup 3 [(3, "hello")])
  putStrLn $ "added = " ++ show added
  putStrLn $ "y, z, tupled = "
             ++ show y ++ ", "
             ++ show z ++ ", "
             ++ show tupled
  putStrLn $ "maxed = " ++ show maxed
  putStrLn $ "const <$> Identity [1, 2, 3] <*> Identity [9, 9, 9] = " ++
    show (const <$> Identity [1, 2, 3] <*> Identity [9, 9, 9])
  putStrLn $ "Constant (Sum 1) <*> Constant (Sum 2) = " ++
    show (Constant (Sum 1) <*> Constant (Sum 2))
  putStrLn $ "pure 1 :: Const String Int = " ++
    show (pure 1 :: Const String Int)
