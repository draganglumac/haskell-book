module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{- Type substitution from definitions

(.) :: (b -> c) -> (a -> b) -> (a -> c)
--      fmap        fmap
fmap :: Functor f => (m -> n) -> f m -> f n
fmap :: Functor g => (x -> y) -> g x -> g y

(b -> c) in (.) <=> (m -> n) -> f m -> f n
  b = (m -> n)
  c = (f m -> f n)

plug into definition of composition
(.) :: Functor f =>
   ((m -> n) -> (f m -> f n)) -- left operand
-> (a -> (m -> n))            -- right operand
-> (a -> f m -> f n)          -- result

by the same token
(a -> b) in (.) <=> (x -> y) -> g x -> g y
  a = (x -> y)
  b = (g x -> g y)

and from Functor f b = (m -> n), i.e.
  (m -> n) = (g x -> g y)
  m = g x
  n = g y
plug into definition of composition

(.) :: (Functor f, Functor g) =>
   ((g x -> g y) -> (f (g x) -> f (g y)) -- left operand  (fmap outter)
-> (x -> y) -> (g x -> g y)              -- right operand (fmap inner)
-> (x -> y) -> f (g x) -> f (g y)        -- result

therefore, the fmap composition will have the result type

fmap . fmap :: (Functor f, Functor g) => (x -> y) -> f (g x) -> f (g y)

-}
