{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module ProgrammingInTypes.ParameterizedTypes where

import qualified Data.Map as Map

data Box a = Box a deriving (Show, Eq)

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

data Triple a = Triple a a a deriving (Show, Eq)

first (Triple x _ _ ) = x
second (Triple _ y _ ) = y
third (Triple _ _ z ) = z

type Point3D = Triple Double

toList :: Triple a -> [a]
toList (Triple x y z) = [x, y, z]

transform :: (a -> b) -> Triple a -> Triple b
transform f (Triple x y z) = Triple (f x) (f y) (f z)

data List a = Empty | Cons a (List a) deriving (Show, Eq)

myMap :: (a -> b) -> List a -> List b
myMap fn Empty = Empty
myMap fn (Cons a rest) = Cons (fn a) (myMap fn rest)

---------------
-- Using Map --
---------------

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs = zip ids organs

organCatalog = Map.fromList organPairs


