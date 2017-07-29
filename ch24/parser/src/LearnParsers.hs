module LearnParsers where

import Text.Trifecta
import Text.Parser.Combinators (eof)

stop :: Parser a
stop = unexpected "stop"

-- char :: Char -> Parser Char
-- char c =
--   Parser $ \s -> case s of
--                    (x : xs) -> if c == x then [(c, xs)] else []
--                    _        -> []

one = char '1'
one' = one >> stop

oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop

s1 = string "1"
s1' = s1 >> stop

s12 = string "12"
s12' = s12 >> stop

s123 = string "123"
s123' = s123 >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString (p >> eof) mempty "123"

test12 :: Parser Char -> IO ()
test12 p =
  print $ parseString (p >> eof) mempty "12"

testString :: Parser String -> IO ()
testString p =
  print $ parseString p mempty "123"

pNL s =
  putStrLn ('\n' : s)

main :: IO ()
main = do
  pNL "stop:"
  testParse stop
  -- char
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "test12 oneTwo"
  test12 oneTwo
  pNL "test12 oneTwo'"
  test12 oneTwo'
  -- string
  pNL "s1"
  testString s1
  pNL "s1'"
  testString s1'
  pNL "s12"
  testString s12
  pNL "s12'"
  testString s12'
  pNL "s123"
  testString s123
  pNL "s123'"
  testString s123'
