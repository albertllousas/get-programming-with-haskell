{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module ProgrammingInTypes.MaybeType where

import Prelude hiding (Maybe,Just,Nothing)

data Maybe a = Nothing | Just a deriving (Show, Eq, Ord)

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething (Just x) = True

filterJustOrgans :: [Maybe Organ] -> [Maybe Organ]
filterJustOrgans organs = filter isSomething organs

showOrgan :: Maybe Organ -> String
showOrgan Nothing = ""
showOrgan (Just organ) = show organ

data Container = Vat Organ | Cooler Organ | Bag Organ  deriving Eq

instance Show Container where
    show (Vat organ) = show organ ++ " in a vat"
    show (Cooler organ) = show organ ++ " in a cooler"
    show (Bag organ) = show organ ++ " in a bag"

data Location = Lab | Kitchen | Bathroom deriving (Show, Eq)

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer anyOtherOrgan = Bag anyOtherOrgan

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat o) = (Lab, Vat o)
placeInLocation (Cooler o) = (Lab, Cooler o)
placeInLocation (Bag o) = (Kitchen, Bag o)

process :: Organ -> (Location, Container)
process organ = placeInLocation (organToContainer organ)
