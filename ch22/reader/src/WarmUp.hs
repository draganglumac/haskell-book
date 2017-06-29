module WarmUp where

import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . reverse

fmapped :: [Char] -> [Char]
fmapped = fmap cap reverse

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> reverse

tupled' :: [Char] -> ([Char], [Char])
tupled' = (,) <$> reverse <*> cap

monadic :: [Char] -> ([Char], [Char])
monadic = do
  fst <- cap
  snd <- reverse
  return (fst, snd)

monadic' :: [Char] -> ([Char], [Char])
monadic' = do
  fst <- cap
  snd <- reverse
  return (snd, fst)

warmUp :: IO ()
warmUp = do
  putStrLn $ "     cap \"asdf\" = " ++ (cap "asdf")
  putStrLn $ " reverse \"asdf\" = " ++ (reverse "asdf")
  putStrLn "Composition and Functorial contexts:"
  putStrLn $ "composed \"asdf\" = " ++ (composed "asdf")
  putStrLn $ " fmapped \"asdf\" = " ++ (fmapped "asdf")
  putStrLn "Tupled in Applicative and Monadic contexts:"
  putStrLn $ "  tupled \"asdf\" = " ++ show (tupled "asdf")
  putStrLn $ " tupled' \"asdf\" = " ++ show (tupled' "asdf")
  putStrLn $ " monadic \"asdf\" = " ++ show (monadic "asdf")
  putStrLn $ "monadic' \"asdf\" = " ++ show (monadic' "asdf")
