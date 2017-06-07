module ApplicativeLaws where

{- Identity Law
  pure id <*> v = v
-}
identityLaw :: IO ()
identityLaw = do
  print $ pure id <*> [1..5]
  print $ pure id <*> Just "Hello Applicative"
  print $ pure id <*> (Nothing :: Maybe Int)
  print $ pure id <*> (Left "Error'ish" :: Either String Int)
  print $ pure id <*> (Right 8001 :: Either String Int)
  print $ pure id <*> (+1) $ 2
  print $ id [1..5]
  print $ id <$> [1..5]
  print $ pure id <*> [1..5]

{- Composition Law

  Result of composing functions first then applying them is the same as
  the result of applying functions first then composing them.

  pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-}

compositionLaw :: IO ()
compositionLaw = do
  print $ pure (.) <*> [(+1)] <*> [(*2)] <*> [1, 2, 3]
  print $ [(+1)] <*> ([(*2)] <*> [1, 2, 3])
  print $ pure (.) <*> Just (+1) <*> Just (*2) <*> Just 1
  print $ Just (+1) <*> (Just (*2) <*> Just 1)

{- Homomorphism Law

  The effect of applying a function that is embedded in some structure to a
  value that is embedded in some structure, should be the same as applying
  a function to a value without affecting any outside structure.

  pure f <*> pure x = pure (f x)
-}

homomorphismLaw :: IO ()
homomorphismLaw = do
  print $ (pure (+1) <*> pure 1 :: [Int])
  print $ (pure ((+1) 1) :: [Int])
  print (pure (+1) <*> pure 1 :: Maybe Int)
  print (pure ((+1) 1) :: Maybe Int)
  print $ ()

{- Interchange Law

  u <*> pure y = pure ($ y) <*> u
-}

interchangeLaw :: IO ()
interchangeLaw = do
  print $ Just (+2) <*> (pure 2 :: Maybe Int)
  print $ (pure ($ 2) :: Maybe ((Int -> Int) -> Int)) <*> Just (+2)
  print $ [(+1), (*2)] <*> (pure 1 :: [Int])
  print $ (pure ($ 1) :: [((Int -> Int) -> Int)]) <*> [(+1), (*2)]

mPure :: a -> Maybe a
mPure = pure

embed :: Num a => Maybe ((a -> b) -> b)
embed = mPure ($ 2)

mApply :: Maybe ((a -> b) -> b)
       -> Maybe (a -> b)
       -> Maybe b
mApply = (<*>)

myResult = embed `mApply` Just (+ 2)
