module ProgrammingInTypes.SemigroupsAndMonoidsSpec where

import Test.Hspec
import ProgrammingInTypes.SemigroupsAndMonoids

spec :: Spec
spec = do
  describe "composing functions" $ do
    it "should get the last element of a list" $ do
      let list = [1,2,3]
      myLast list `shouldBe` 3
      myLast' list `shouldBe` 3

    it "should get the min element of a list" $ do
      myMin ['b', 'a', 'c'] `shouldBe` 'a'

    it "should get the max element of a list" $ do
      myMax [1, 3, 2] `shouldBe` 3

    it "should check if a property is true of all elements of an array" $ do
      myAll even [1,2,3] `shouldBe` False

  describe "Combining types with Semigroup" $ do
    it "should combine colors" $ do
      Red `combine` Yellow `shouldBe` Orange
      Red `combine` Blue `shouldBe` Purple
      Green `combine` Purple `shouldBe` Brown

    it "should combine colors keeping associativity" $ do
      (Green `combine` Blue) `combine` Yellow `shouldBe` Green
      Green `combine` (Blue `combine` Yellow) `shouldBe` Green

    it "should combine Sum types" $ do
       (Sum 1) `combine` (Sum 2) `shouldBe` (Sum 3)

    it "should combine Product types" $ do
       (Product 3) `combine` (Product 3) `shouldBe` (Product 9)

  describe "Combining types with Monoids" $ do
    it "should `combine` lists using our own definition of monoid" $ do
       ([1,2,3] `combine` identity `combine` [3,2,1]) `shouldBe` [1,2,3,3,2,1]

    it "should `combine` lists using Haskell monoid" $ do
       ([1,2,3] `mappend` mempty `mappend` [3,2,1]) `shouldBe` [1,2,3,3,2,1]

    it "should concat array elements using monoid" $ do
       concat' ["Does", " it", " make", " sense?"] `shouldBe` "Does it make sense?"

    it "should follow the monoid Laws" $ do
       identity `combine` [1,2,3] `shouldBe` [1,2,3] -- left identity
       [1,2,3] `combine` identity `shouldBe` [1,2,3] -- right identity
       (([1] `combine` [2]) `combine` [3]) `shouldBe` ([1] `combine` ([2] `combine` [3])) -- associativity
