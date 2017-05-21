module SignalingAdversity where

import BinaryTree

-- Natural numbers

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger n = case n of
                 Zero   -> 0
                 Succ x -> 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0     = Nothing
  | otherwise = Just (go i) where
                  go n
                    | n == 0    = Zero
                    | otherwise = Succ (go (n - 1))

-- Small library for Maybe
--------------------------
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing       _ = False

isJust :: Maybe a -> Bool
isJust = not . isNothing

-- Maybe catamorphism
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing  = b
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing   = a
fromMaybe _ (Just a') = a'

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\m as -> case m of
                            Nothing -> as
                            Just a  -> a : as) []

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe ms = let as = catMaybes ms in
                 if length as < length ms
                 then Nothing
                 else Just as

-- Small library for Either
---------------------------
lefts' :: [Either a b] -> [a]
lefts' = foldr (\e as -> case e of
                         Left a -> a : as
                         _      -> as) []

rights' :: [Either a b] -> [b]
rights' = foldr (\e bs -> case e of
                          Right b -> b : bs
                          _       -> bs) []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr (\e (as, bs) -> case e of
                                         Left a  -> (a : as, bs)
                                         Right b -> (as, b : bs)) ([], [])

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f e = case e of
                   Left a  -> Nothing
                   Right b -> Just (f b)

-- general catamorphism for Either values
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' af bf e = case e of
                  Left a  -> af a
                  Right b -> bf b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' bf = either' (\_ -> Nothing) (\b -> Just (bf b))

-- anamorphisms - iterate and unfoldr
myIterate :: (a -> a) -> a -> [a]
myIterate fa a = a : myIterate fa (fa a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
                Nothing      -> []
                Just (a, b1) -> a : myUnfoldr f b1

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\a -> Just (a, f a))

-- BinaryTree stuff

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case f a of
             Nothing         -> Leaf
             Just (a, b, a1) -> Node (unfold f a) b (unfold f a1)

treeBuild :: Integer -> BinaryTree Integer
treeBuild = unfold (\i -> if i == 0
                          then Nothing
                          else Just (i-1, i-1, i-1))
