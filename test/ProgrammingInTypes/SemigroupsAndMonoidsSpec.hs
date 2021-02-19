module ProgrammingInTypes.SemigroupsAndMonoidsSpec where

import Test.Hspec
import ProgrammingInTypes.SemigroupsAndMonoids
import Prelude hiding ((<>))

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
      Red <> Yellow `shouldBe` Orange
      Red <> Blue `shouldBe` Purple
      Green <> Purple `shouldBe` Brown

    it "should combine colors keeping associativity" $ do
      (Green <> Blue) <> Yellow `shouldBe` Green
      Green <> (Blue <> Yellow) `shouldBe` Green