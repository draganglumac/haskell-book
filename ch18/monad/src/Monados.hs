-- src/Monados.hs
module Monados where

import Prelude hiding (
                        (>>=)
                      , (>>)
                      , return
                      )

class Applicative m => Monados m where
  -- `bind` - this is the magic-of-Monad operator and it makes Monad special
  (>>=) :: m a -> (a -> m b) -> m b
  infixl 1 >>=
  -- no official name, unofficially `the sequence operator`
  -- sequences two actions while discarding any resulting value
  -- of the first action
  (>>) :: m a -> m b -> m b
  infixl 1 >>
  -- `return` is the same as `pure`
  return :: a -> m a

fmap :: Monados m => (a -> b) -> m a -> m b
fmap f xs = xs >>= return . f

{- Chain of dependency

  Functor -> Applicative -> Monad

  This means that whenever you implement an instance of Monad
  you automatically get Applicative and Functor as well.

  Comparison of similar operations in type classes:

  fmap :: Functor f     => (a -> b)   -> f a        -> f b
  <*>  :: Applicative f => f (a -> b) -> f a        -> f b
  >>=  :: Monad f       => f a        -> (a -> f b) -> f b
-}

-- this is the unique part of Monad
-- it generalises `concat` :: [[t]] -> [t]
-- it's imported from Control.Monad
join :: Monados m => m (m a) -> m a
join mma = mma >>= id
