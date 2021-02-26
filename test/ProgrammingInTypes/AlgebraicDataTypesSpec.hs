module ProgrammingInTypes.AlgebraicDataTypesSpec where

import Test.Hspec
import ProgrammingInTypes.AlgebraicDataTypes

spec :: Spec
spec = do
  describe "Breakfast menu" $ do
    it "should construct a breakfast menu" $ do
      show (KidsBreakfast Egg HomeFries) `shouldBe` "KidsBreakfast Egg HomeFries"

  describe "book store" $ do
    let book = BookItem Book {
                        author    = Author (Name "Stephen" "King"),
                        isbn      = "0-670-81302-8",
                        bookTitle = "It",
                        bookYear  = 1986,
                        bookPrice = Price 20.5
                        }
    let record = RecordItem VinylRecord {
                            artist      = Band "Queen",
                            recordTitle = "Greatest Hits",
                            recordYear  = 1981,
                            recordPrice = Price 60.5
                            }
    let toy = ToyItem Collectible {
                            name              = "Furby",
                            description       = "American electronic robotic toy.",
                            collectiblePrice  = Price 100.0
                            }
    it "should get the price for store items" $ do
      map price [book, record, toy] `shouldBe` [20.5, 60.5, 100.0]

    it "should get who made store items" $ do
      map madeBy [book, record, toy] `shouldBe` ["Author (Name \"Stephen\" \"King\")","Band \"Queen\"","unknown"]
