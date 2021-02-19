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

class Semigroup' a where
  (<>) :: a -> a -> a -- <> is an associative operation (combine)

data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown deriving (Eq, Show)

instance Semigroup' Color where
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Yellow Red = Orange
    (<>) Red Yellow = Orange
    (<>) a b | a == b = a
             | [a,b] `areIn` [Red,Blue,Purple] = Purple
             | [a,b] `areIn` [Blue,Yellow,Green] = Green
             | [a,b] `areIn` [Red,Yellow,Orange] = Orange
             | otherwise = Brown
             where areIn = \items inValues ->  all (`elem` (inValues::[Color])) (items::[Color])
