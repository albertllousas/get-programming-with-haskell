module IntroducingTypes.TypeClassesSpec where

import Test.Hspec
import IntroducingTypes.TypeClasses
import Data.List

spec :: Spec
spec = do
  describe "type classes" $ do

    it "should show a six sided die" $ do
      map show [S1, S2, S3, S4, S5, S6] `shouldBe` ["one", "two", "three","four","five","six"]

    it "should check the equality between two sides of a six sided die" $ do
      S1 == S1 `shouldBe` True
      S1 /= S2 `shouldBe` True

    it "should convert num to a six sided die" $ do
      map toEnum [0,1,2,3,4,5] `shouldBe` [S1, S2, S3, S4, S5, S6]

    it "should convert a six sided die to a num" $ do
      map fromEnum [S1, S2, S3, S4, S5, S6] `shouldBe` [0,1,2,3,4,5]

    it "should implement eq, ord, enum and show using `deriving` for an icecream data type" $ do
      show Vainilla `shouldBe` "Vainilla"
      Vainilla == Vainilla `shouldBe` True
      Vainilla /= Chocolate `shouldBe` True
      fromEnum Vainilla `shouldBe` 0
      Vainilla < Chocolate `shouldBe` True

    it "should sort an array of names" $ do
      let names = [Name ("John", "Doe"), Name ("Jane", "Doe"), Name ("John", "Smith")]
      sort names `shouldBe` [Name ("Jane","Doe"),Name ("John","Doe"),Name ("John","Smith")]

