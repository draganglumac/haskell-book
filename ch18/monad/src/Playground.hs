module Playground where

import Control.Monad

-- write bind in terms of fmap and join
bind :: Monad m => (a -> m b) -> m a -> m b
bind f xs = join $ fmap f xs

addOne :: Int -> [Int]
addOne x = [x, 1]

honkIfYouLikeBind :: IO ()
honkIfYouLikeBind = do
  putStrLn "bind addOne [1..10]"
  print $ bind addOne [1..10]
  putStrLn "[1..10] >>= addOne"
  print $ [1..10] >>= addOne

sequencing :: IO ()
sequencing = do
  putStrLn "Hello"
  putStrLn "World"

sequencing' :: IO ()
sequencing' =
  putStrLn "Hello" *> putStrLn "World"

sequencing'' :: IO ()
sequencing'' =
  putStrLn "Hello" >> putStrLn "World"

binding :: IO ()
binding = do
  print "Your name: "
  name <- getLine
  putStrLn $ "Hello, " ++ name

binding' :: IO ()
binding' = print "Your name: " >> getLine >>= \name -> putStrLn $ "Hello, " ++ name

badoomBadoom :: IO ()
badoomBadoom = do
  sequencing
  sequencing'
  sequencing''
  binding
  binding'
  bindingAndSequencing
  bindingAndSequencing'
  twoBinds
  twoBinds'

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y halo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls: " >>
  getLine >>=
  \name -> putStrLn ("y halo thar: " ++ name)

twoBinds :: IO ()
twoBinds = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn "age pls:"
  age <- getLine
  putStrLn ("y halo thr: " ++ name ++ " who is: " ++ age ++ " yrs old.")

twoBinds' :: IO ()
twoBinds' =
  putStrLn "name pls:" >>
  getLine >>=
  \name -> putStrLn "age pls:" >>
  getLine >>=
  \age -> putStrLn ("y halo thr: " ++ name ++ " who is: " ++ age ++ " yrs old.")
