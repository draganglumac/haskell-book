-- the following will export all top leve bindings
-- module Hello where
--
-- but, we want to show how to explicity export selected bindings
-- spaces next to parentheses seem to be required [DG]
module Hello
  ( sayHello )
  where

sayHello  :: IO ()
sayHello = do
  putStrLn "hello world"
