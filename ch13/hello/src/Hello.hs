-- the following will export all top leve bindings
-- module Hello where
--
-- but, we want to show how to explicity export selected bindings
-- spaces next to parentheses are not required they're just added for clarity [DG]
module Hello
  ( sayHello )
  where

sayHello  :: String -> IO ()
sayHello name = putStrLn ("Hello " ++ name ++ "!")
