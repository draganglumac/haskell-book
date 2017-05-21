{-# LANGUAGE DuplicateRecordFields #-}

module AlgebraicDataTypes where

data Doggies a =
    Husky a
  | Mastif a
  deriving (Eq, Show)

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline deriving (Eq, Show)

myCar :: Vehicle
myCar = Car Mini (Price 14000)

urCar :: Vehicle
urCar = Car Mazda (Price 20000)

clownCar :: Vehicle
clownCar = Car Tata (Price 7000)

dodge :: Vehicle
dodge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _         = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _         = undefined

-- newtype - a type that has a single unary data constructor and nothing else
-- optimised at compile time to the argument type of the data constructor so no runtime overhead
tooManyGoatsPlain :: Int -> Bool
tooManyGoatsPlain n = n > 42

-- now we are guaranteed that goats and cows won't get mixed up
newtype Goats = Goats Int deriving (Eq, Show)
newtype Cows = Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

-- product types
data Person = MkPerson String Int deriving (Eq, Show)

namae :: Person -> String
namae (MkPerson s _) = s

-- record syntax gives us named accessors
data PersonRec =
  PersonRec { name :: String
            , age :: Int }
            deriving (Eq, Show)

-- so now we can do things like
-- Prelude> let papu = Person "Papu" 5
-- Prelude> name papu
-- "Papu"
-- Prelude> age papu
-- 5

-- Constructing and deconstructing values
data GuessWhat = Chickenbutt deriving (Eq, Show)
data Id a = MkId a deriving (Eq, Show)
data Product a b = Product a b deriving (Eq, Show)
data Sum a b = First a | Second b deriving (Eq, Show)
data RecordProduct a b = RecordProduct { pfirst :: a, psecond :: b } deriving (Eq, Show)
-- Sum and product
newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)

data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
type Farmhouse' = Product NumCow NumPig

newtype NumSheep = NumSheep Int deriving (Eq, Show)

data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)

type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

-- Exercise: Programmers

data OperatingSystem = GnuPlusLinux
                     | OpenBSDPlusNevermindJustBSDStill
                     | Mac
                     | Windows
                     deriving (Eq, Show, Enum)

data ProgrammingLanguage = Haskell
                         | Agda
                         | Idris
                         | PureScript
                         deriving (Eq, Show, Enum)

data Programmer = Programmer { os :: OperatingSystem
                             , lang :: ProgrammingLanguage }
                             deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = enumFromTo GnuPlusLinux Windows

allLanguages :: [ProgrammingLanguage]
allLanguages = enumFromTo Haskell PureScript

allProgrammers :: [Programmer]
allProgrammers = [Programmer { os = anOs, lang = aLang } | anOs <- allOperatingSystems, aLang <- allLanguages]

-----------------------
-- Deconstructing types
-----------------------
newtype Name = Name String deriving Show
newtype Acres =Acres Int deriving Show

-- FarmerType is a Sum
data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer deriving Show

-- Farmer is a plain ole product of -- Name, Acres, and FarmerType
data Farmer = Farmer Name Acres FarmerType deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

-- alternate form with a record syntax
data FarmerRec =
  FarmerRec { name :: Name
            , acres :: Acres
            , farmerType :: FarmerType }
            deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer = case farmerType farmer of
                          DairyFarmer -> True
                          _ -> False

