module ProgrammingInTypes.ParameterizedTypesSpec where

import Test.Hspec
import ProgrammingInTypes.ParameterizedTypes
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "A box of things" $ do
    it "should wrap and unwrap something in a box" $ do
      let box = wrap "something"
      box `shouldBe` (Box "something")
      unwrap box `shouldBe` "something"

  describe "A triple" $ do
    it "should store 3 elements" $ do
      let point3D = (Triple 1.0 2.0 3.0)::Point3D
      first point3D `shouldBe` 1.0
      second point3D `shouldBe` 2.0
      third point3D `shouldBe` 3.0

    it "should convert a triple to a list" $ do
      toList (Triple "a" "b" "c") `shouldBe` ["a", "b", "c"]

    it "should transform a triple" $ do
      transform (* 3) (Triple 1 2 3) `shouldBe` (Triple 3 6 9)
      transform reverse (Triple "Howard" "Philip" "Trevor") `shouldBe` (Triple "drawoH" "pilihP" "roverT")

  describe "Own List implementation" $ do
    it "should map over a list'" $ do
      let list = Cons 1 (Cons 2 (Cons 3 Empty))
      myMap (+ 1) list `shouldBe` Cons 2 (Cons 3 (Cons 4 Empty))

  describe "Using Map" $ do
    it "should lookup for an organ" $ do
      Map.lookup 7 organCatalog `shouldBe` (Just Heart)
