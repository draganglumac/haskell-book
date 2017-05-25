module WordNumber
  ( digitToWord
  , digits
  , wordNumber )
  where

import qualified Data.Map as M

table :: M.Map Integer String
table = M.fromList [
    (0, "zero")
  , (1, "one")
  , (2, "two")
  , (3, "three")
  , (4, "four")
  , (5, "five")
  , (6, "six")
  , (7, "seven")
  , (8, "eight")
  , (9, "nine") ]

digitToWord :: Integer -> String
digitToWord i = case M.lookup i table of
                Just word -> word
                _         -> error (show i ++ " is not a digit.")

digits :: Integer -> [Integer]
digits n = go n [] where
             go num digits = if num < 10
                             then num : digits
                             else go (num `div` 10) ((num `mod` 10) : digits)

wordNumber :: Integer -> String
wordNumber n = foldr
                 (\w ws -> if ws == "" then w ++ ws else w ++ "-" ++ ws)
                 ""
                 $ map (digitToWord) $ digits n
