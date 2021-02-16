{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module ProgrammingInTypes.AlgebraicDataTypes where

--------------------
-- Breakfast menu --
--------------------

data Side = Toast | Biscuit | HomeFries | Fruit deriving Show
data Meat = Sausage | Bacon | Ham deriving Show
data Main = Egg | Pancake | Waffle deriving Show

data BreakFastSpecial =
    KidsBreakfast Main Side
  | BasicBreakfast Main Meat Side
  | LumberJack Main Main Meat Meat Side Side Side

----------------
-- Book Store --
----------------
-- Algebraic data types: https://wiki.haskell.org/Algebraic_data_type

type FirstName = String
type LastName = String
type MiddleName = String

data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName deriving (Show)

data Author = Author Name deriving (Show)

data Artist = Artist Name | Band String deriving (Show)

newtype Price = Price Double

data Book = Book {
     author        :: Author
   , isbn          :: String
   , bookTitle     :: String
   , bookYear      :: Int
   , bookPrice     :: Price
}

data VinylRecord = VinylRecord {
      artist         :: Artist
    , recordTitle    :: String
    , recordYear     :: Int
    , recordPrice    :: Price
}

data Collectible = Collectible {
      name             :: String
    , description      :: String
    , collectiblePrice :: Price
}

data StoreItem  = BookItem Book | RecordItem VinylRecord | ToyItem Collectible

data BookStore = BookStore [StoreItem]

price :: StoreItem -> Double -- three diff ways to get the value
price (RecordItem VinylRecord { recordPrice = Price p }) = p
price (BookItem book) = p where Price p = bookPrice book
price toy = p where (ToyItem Collectible { collectiblePrice = Price p }) = toy

madeBy :: StoreItem -> String
madeBy (BookItem book) = show (author book)
madeBy (RecordItem vinyl) = show (artist vinyl)
madeBy _ = "unknown"
