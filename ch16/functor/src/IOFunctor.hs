module IOFunctor where

-- getLine :: IO String
-- read :: Read a => String -> a

getInt :: IO Int
getInt = fmap read getLine

{-
    A way you can read getLine here is that it's not a String, but rather
  a way to obtain a String.
    IO does not guarantee that effects will be performed, but it does mean
  that they could be performed. Here the side effect needs to block and wait
  for user input via the standard input stream the operating system provides.
-}
