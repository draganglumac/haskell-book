module NewTypes where

-- registered user
newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name)
                          (AccountNumber acctNum))
          = putStrLn $ name ++ " " ++ show acctNum

-- penguins
data WherePenguinsLive =
    Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)

-- is it South Africa? If so, return True
isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica           _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

humboldt  = Peng SouthAmerica
gentoo    = Peng Antarctica
macaroni  = Peng Antarctica
little    = Peng Australia
galapagos = Peng Galapagos

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin                _ = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin                 _ = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p =
  (galapagosPenguin p) || (antarcticPenguin p)

-- tuples
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))

-- These have to be the same type because (+) is a -> a -> a
addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

addEmUp2Alt :: Num a => (a, a) -> a
addEmUp2Alt tup = (fst tup) + (snd tup)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, x) = x

-- case expressions
funcZ x =
  case x + 1 == 1 of
    True  -> "Awesome"
    False -> "wut"

nums x =
  case compare x 0 of
    LT -> -1
    EQ -> 0
    GT -> 1

-- higher order functions
data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: Employee -> Employee -> IO ()
employeeRank e e' =
  case compare e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> (flip reportBoss) e e'

flexiRank :: (Employee -> Employee -> Ordering)
          -> Employee
          -> Employee
          -> IO ()
flexiRank f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> (flip reportBoss) e e'

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder     _ = GT
codersRuleCEOsDrool _     Coder = LT
codersRuleCEOsDrool e e'        = compare e e'

-- composition
-- pointfree style (i.e. no arguments)
firstFiveOdd :: Integral a => a -> [a]
firstFiveOdd = take 5 . filter odd . enumFrom

-- point style
firstFiveOdd' :: Integral a => a -> [a]
firstFiveOdd' a = take 5 . filter odd . enumFrom $ a

-- pointfree
addWithZel :: Int -> [Int] -> Int
-- addWithZel z xs = foldr (+) z xs
addWithZel = foldr (+)

-- print and putStrLn
myPrintPF :: Show a => a -> IO ()
myPrintPF = putStrLn . show

myPrint :: Show a => a -> IO ()
myPrint a = putStrLn . show $ a
