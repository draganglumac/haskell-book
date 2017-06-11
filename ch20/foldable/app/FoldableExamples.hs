module FoldableExamples where

import FoldableInstances
import Data.Monoid

identityExamples :: IO ()
identityExamples = do
  putStrLn $
    "`foldr (*) 5 (Identity 5)` = "
     <> show (foldr (*) 5 (Identity 5))
  putStrLn $
    "`foldMap (*5) (Identity 100) :: Product Integer` = "
    <> show (foldMap (*5) (Identity 100) :: Product Integer)

tryExamples :: IO ()
tryExamples = do
  putStrLn $
    "`foldMap (+1) None :: Sum Int` = "
    <> show (foldMap (+1) None :: Sum Int)
  putStrLn $
    "`foldMap (+1) None :: Product Int` = "
    <> show (foldMap (+1) None :: Product Int)
  putStrLn $
    "`foldMap (+1) (Just 1) :: Sum Int` = "
    <> show (foldMap (+1) (Just 1) :: Sum Int)
