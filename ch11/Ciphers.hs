module Ciphers where

import Data.Char
import Data.List

type KeyWord = String
type Message = String

data State = State KeyWord Int deriving (Eq, Show)
type StateAction = State -> Char -> (State, Char)
type ShiftFunction = Char -> Int -> Char


--vigenereDecode :: KeyWord -> Message -> String

aToZ :: String
aToZ = ['A'..'Z']

upcase :: String -> String
upcase = map toUpper

translate :: ShiftFunction -> State -> Char -> (State, Char)
translate shift s@(State k i) c = case (elemIndex (k !! i) aToZ) of
  Just n -> if elem c aToZ
            then (State k (mod (i + 1) (length k)), shift c n)
            else (s, c)
  _      -> (s, c)

encode :: StateAction
encode = translate (\c n -> case elemIndex c aToZ of
                            Just i -> aToZ !! (mod (i + n) 26))

decode :: StateAction
decode = translate (\c n -> case elemIndex c aToZ of
                            Just i -> aToZ !! (mod (26 + i - n) 26))

vigenere :: StateAction -> KeyWord -> Message -> (State, String)
vigenere action k m = foldr
                      (\c acc -> let (s1, next) = action (fst acc) c in (s1, (snd acc) ++ [next]))
                      (State (upcase k) 0, "")
                      (upcase (reverse m))

vigenereEncode :: KeyWord -> Message -> String
vigenereEncode k m = snd (vigenere encode k m)

vigenereDecode :: KeyWord -> Message -> String
vigenereDecode k m = snd (vigenere decode k m)
