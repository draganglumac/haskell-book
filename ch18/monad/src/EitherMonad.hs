module EitherMonad where

-- years ago
type Founded = Int

-- how many
type Coders = Int

data SoftwareShop =
  Shop {
    founded :: Founded
  , programmers :: Coders
  } deriving (Eq, Show)

data SoftwareShopError =
    NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either SoftwareShopError Founded
validateFounded n | n < 0     = Left $ NegativeYears n
                  | n > 500   = Left $ TooManyYears n
                  | otherwise = Right n

validateCoders :: Int -> Either SoftwareShopError Coders
validateCoders n | n < 0      = Left $ NegativeCoders n
                 | n > 5000   = Left $ TooManyCoders n
                 | otherwise  = Right n

mkSoftware :: Int -> Int -> Either SoftwareShopError SoftwareShop
mkSoftware years coders = do
  yrs  <- validateFounded years
  cdrs <- validateCoders coders
  if cdrs > div yrs 10
    then Left $ TooManyCodersForYears yrs cdrs
    else Right $ Shop yrs cdrs


softwareShop :: IO ()
softwareShop = do
  putStrLn "mkSoftware 0 0"
  putStrLn $ show $ mkSoftware 0 0
  putStrLn "mkSoftware (-1) 0"
  putStrLn $ show $ mkSoftware (-1) 0
  putStrLn "mkSoftware (-1) (-1)"
  putStrLn $ show $ mkSoftware (-1) (-1)
  putStrLn "mkSoftware 0 (-1)"
  putStrLn $ show $ mkSoftware 0 (-1)
  putStrLn "mkSoftware 500 0"
  putStrLn $ show $ mkSoftware 500 0
  putStrLn "mkSoftware 501 0"
  putStrLn $ show $ mkSoftware 501 0
  putStrLn "mkSoftware 501 501"
  putStrLn $ show $ mkSoftware 501 50
  putStrLn "mkSoftware 100 5001"
  putStrLn $ show $ mkSoftware 100 5001
  putStrLn "mkSoftware 0 500"
  putStrLn $ show $ mkSoftware 0 500

-- implement Either Monad
data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First x) = First x
  fmap f (Second y) = Second $ f y

instance Applicative (Sum a) where
  pure x = Second x
  (<*>) _ (First x) = First x
  (<*>) (First f) _ = First f
  (<*>) (Second f) (Second y) = Second $ f y

instance Monad (Sum a) where
  return = pure
  (>>=) (First x) _ = First x
  (>>=) (Second y) f = f y
