module MaybeMonad where

data Cow = Cow {
    name   :: String
  , age    :: Int
  , weight :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty ""  = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0    = Just n
             | otherwise = Nothing

-- if Cow's name is Bess, must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
     then Nothing
     else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' = do
  nammy   <- noEmpty name'
  agey    <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)

-- without do sugar
mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' n a w =
  case noEmpty n of
    Nothing -> Nothing
    Just nammy ->
      case noNegative a of
        Nothing -> Nothing
        Just agey ->
          case noNegative w of
            Nothing -> Nothing
            Just weighty ->
              weightCheck (Cow nammy agey weighty)

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' n a w =
  noEmpty n >>=
  \nammy   -> noNegative a >>=
  \agey    -> noNegative w >>=
  \weighty -> weightCheck (Cow nammy agey weighty)

makeSomeCows :: IO ()
makeSomeCows = do
  putStrLn "mkSphericalCow \"Bess\" 5 499"
  putStrLn $ show $ mkSphericalCow "Bess" 5 499
  putStrLn "mkSphericalCow' \"Bess\" 5 499"
  putStrLn $ show $ mkSphericalCow' "Bess" 5 499
  putStrLn "mkSphericalCow'' \"Bess\" 5 499"
  putStrLn $ show $ mkSphericalCow'' "Bess" 5 499
