module ProgrammingInTypes.MaybeTypeSpec where

import Test.Hspec
import ProgrammingInTypes.MaybeType
import Prelude hiding (Maybe,Just,Nothing)

spec :: Spec
spec = do
  describe "Organs with maybes" $ do
    it "should filter a list of maybe organs to a organs" $ do
      let availableOrgans = [Nothing, Just Heart, Nothing, Nothing, Nothing, Just Kidney]
      filterTheOrgans availableOrgans `shouldBe` [Just Heart, Just Kidney]

    it "should show maybe an organ" $ do
      showOrgan (Just Kidney) `shouldBe` "Kidney"
      showOrgan Nothing `shouldBe` ""

    it "should process an organ putting it in the proper container and location" $ do
      process Brain `shouldBe` (Lab, Vat Brain)
      process Heart `shouldBe` (Lab, Cooler Heart)
      process Spleen `shouldBe` (Kitchen, Bag Spleen)
      process Kidney `shouldBe` (Kitchen, Bag Kidney)