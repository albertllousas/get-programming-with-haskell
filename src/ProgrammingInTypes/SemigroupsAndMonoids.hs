{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module ProgrammingInTypes.SemigroupsAndMonoids where

import Data.List

---------------------------
-- Compose combining fns --
----------------------------

myLast :: [a] -> a
myLast l = head (reverse l)

myLast' :: [a] -> a
myLast' = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

myMax :: Ord a => [a] -> a
myMax = myLast . sort

myAll :: (a -> Bool) -> [a] -> Bool
myAll testFn = (foldl (&&) True) . (map testFn) -- f (g x) = (f . g) x

---------------
-- Semigroup --
---------------

class MySemigroup a where -- a A set of elements that satisfies:
  combine :: a -> a -> a -- Associativity (<>): binary associative op call combine, which "adds" together two elements to make a new one

data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown deriving (Eq, Show)

instance MySemigroup Color where
    combine Red Blue = Purple
    combine Blue Red = Purple
    combine Yellow Blue = Green
    combine Blue Yellow = Green
    combine Yellow Red = Orange
    combine Red Yellow = Orange
    combine a b | a == b = a
             | [a,b] `areIn` [Red,Blue,Purple] = Purple
             | [a,b] `areIn` [Blue,Yellow,Green] = Green
             | [a,b] `areIn` [Red,Yellow,Orange] = Orange
             | otherwise = Brown
             where areIn = \items inValues ->  all (`elem` (inValues::[Color])) (items::[Color])

data Sum a = Sum a deriving (Eq, Ord, Show)

instance Num a => MySemigroup (Sum a) where
    combine (Sum x) (Sum y) = Sum (x + y)

data Product a = Product a deriving (Eq, Ord, Show)

instance Num a => MySemigroup (Product a) where
    combine (Product x) (Product y) = Product (x * y)

------------
-- Monoid --
------------

class (MySemigroup a) => MyMonoid a where -- a A set of elements satisfies Semigroup and:
    identity :: a -- Identity: identity element, unit element which acts neutrally when "added" to any other element in the Set
    concat' :: [a] -> a -- Combine a list of monoids into a single one
    concat' = foldr combine identity -- default impl

instance MySemigroup [a] where combine = (++) -- I need to do this or I cannot create a MyMonoid instance

instance MyMonoid [a] where
    identity  = []
