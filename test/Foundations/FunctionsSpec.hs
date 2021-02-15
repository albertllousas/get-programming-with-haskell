module Foundations.FunctionsSpec where

import Test.Hspec
import Foundations.Functions

spec :: Spec
spec = do
  describe "ifEvenInc fn" $ do
    it "should inc if number is even" $ do
      ifEvenInc 2 `shouldBe` 3

  describe "flipBinaryArgs fn" $ do
    it "should flip the arguments of a binary function" $ do
        let flipDivision = flipBinaryArgs (/)
        flipDivision 4 2 `shouldBe` 0.5

  describe "factorial fn with pattern matching" $ do
    it "should calculate the factorial" $ do
        factorialWithPatternMatching 4 `shouldBe` 24

  describe "factorial fn with guards" $ do
      it "should calculate the factorial" $ do
          factorialWithGuards 4 `shouldBe` 24

  describe "squares fn with where clause" $ do
        it "should calculate the squares of a pair of ints" $ do
            squaresWithWhereClause (2,2) `shouldBe` (4,4)

  describe "squares fn with let in" $ do
          it "should calculate the squares of a pair of ints" $ do
              squaresWithLetIn (2,2) `shouldBe` (4,4)
