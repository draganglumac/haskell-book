module LearnParsers where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = one >> stop

-- char :: Char -> Parser Char
-- char c =
--   Parser $ \s -> case s of
--                    (x : xs) -> if c == x then [(c, xs)] else []
--                    _        -> []

oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"
