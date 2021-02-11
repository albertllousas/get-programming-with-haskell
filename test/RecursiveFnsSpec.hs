module RecursiveFnsSpec where

import Test.Hspec
import RecursiveFns

spec :: Spec
spec = do
  describe "myLength fn" $ do
    it "should calculate the length of an empty string" $ do
      myLength "" `shouldBe` 0
    it "should calculate the length of an string" $ do
      myLength "hello" `shouldBe` 5
    it "should calculate the length of an aray" $ do
      myLength [1,2,3] `shouldBe` 3

  describe "myTake fn" $ do
     it "should take the first 'n' elements of a list" $ do
       myTake 2 "hello" `shouldBe` "he"

  describe "myMap fn" $ do
     it "should map a function to each element of a list" $ do
       myMap ("a "++) ["dog","cat"] `shouldBe` ["a dog", "a cat"]

  describe "myFilter fn" $ do
    it "should filter elements of a list given a function" $ do
      let startsWithD = (\(x:xs) -> x == 'd')
      myFilter startsWithD ["dog","cat"] `shouldBe` ["dog"]

  describe "myFoldLeft fn" $ do
     it "should reduce a list to a single value" $ do
       myFoldLeft (-) 0 [1,2,3,4] `shouldBe` -10

  describe "myFoldRight fn" $ do
     it "should reduce a list to a single value" $ do
       myFoldRight (-) 0 [1,2,3,4] `shouldBe` -2
