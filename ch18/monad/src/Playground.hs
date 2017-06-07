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
