module MonkeyAbout where

import Control.Applicative
boop :: Num a => a -> a
boop = (*2)

doop :: Num a => a -> a
doop = (+10)

-- bip :: Integer -> Integer
bip :: Num a => a -> a
bip = boop . doop

-- Functorial context here is a partially-applied function
-- `bloop` is actually the same as the above `bip`
bloop :: Num a => a -> a
bloop = fmap boop doop

-- Applicative context. This is different to the above.
-- In this context argument gets passed to both functions
-- `boop` and `doop` in parallel, and results are added together.
bbop :: Num a => a -> a
bbop = (+) <$> boop <*> doop

duwop :: Num a => a -> a
duwop = liftA2 (+) boop doop

-- Monadic context here, same as Applicative
boopDoop :: Num a => a -> a
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)

badumDoosh :: IO ()
badumDoosh = do
  putStrLn ("    boop 3 = " ++ show (boop 3))
  putStrLn ("    doop 3 = " ++ show (doop 3))
  putStrLn "Composition and Functorial contexts: "
  putStrLn ("     bip 3 = " ++ show (bip 3))
  putStrLn ("   bloop 3 = " ++ show (bloop 3))
  putStrLn "Applicative and Monadic contexts: "
  putStrLn ("    bbop 3 = " ++ show (bbop 3))
  putStrLn ("   duwop 3 = " ++ show (duwop 3))
  putStrLn ("boopDoop 3 = " ++ show (boopDoop 3))


foo :: (Functor f, Num a) => f a -> f a
foo = fmap (+1)

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

barPlus r =(foo r, length r)

frooty' :: Num a => [a] -> ([a], Int)
frooty' r = bar (foo r) r

-- make it look more Reader'y
frooty :: Num a => [a] -> ([a], Int)
frooty = \r -> bar (foo r) r

-- but recall Monad's bind type
-- (>==) :: Monad m =>
--          m    a  -> a ->  m    b ->   m    b
-- so just like in Applicative, ((->) r) is our Monad structure
fooBind :: (r -> a) -> (a -> r -> b) -> (r -> b)
fooBind m k = \r -> k (m r) r

monaDoosh :: IO ()
monaDoosh = undefined
