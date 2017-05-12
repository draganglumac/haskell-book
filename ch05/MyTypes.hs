module MyTypes where

-- we use the double colon to assign a type
-- making  the type concrete will eliminate
-- the typeclass constraint
addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b + 5

subtractStuff :: Integer -> Integer -> Integer
subtractStuff a b = a - b - 5
subtractOne = subtractStuff 1

nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

curriedFunction :: Integer
                -> Bool
                -> Integer 
curriedFunction i b =
  i + (nonsense b)

uncurriedFunction :: (Integer, Bool)
                  -> Integer
uncurriedFunction (i, b) =
  i + (nonsense b)

anonymous :: Integer -> Bool -> Integer 
anonymous = \i b -> i + (nonsense b)

anonNested :: Integer
           -> Bool
           -> Integer
anonNested =
  \i -> \b -> i + (nonsense b)

-- Sectioning
powerOfTwo :: Integer -> Integer
powerOfTwo = (2^)

squared :: Integer -> Integer
squared = (^2)

celebrate :: String -> String
celebrate = (++ " woot!")

-- parametric and constraing polymorphism
f :: a -> b -> b
f x y = y

f1 :: a -> b -> b
f1 = \x -> \y -> y

