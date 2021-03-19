module WorkingWithTypeInAContext.MonadTypeClassSpec where

import Test.Hspec
import WorkingWithTypeInAContext.MonadTypeClass
import ProgrammingInTypes.MaybeType
import Prelude hiding (Maybe,Just,Nothing,(>>=), (>>), return, fail, fmap)

spec :: Spec
spec = do
  describe "MyMonad" $ do

    it "should increment a maybe" $ do
       inc (Just 1) `shouldBe` Just 2
       inc Nothing `shouldBe` Nothing

    it "should bind a function over a maybe" $ do
      let square = \x -> Just (x ^ 2)
      (Just 2) >>= square `shouldBe` Just 4
      Nothing >>= square `shouldBe` Nothing

    it "should chain bind operations" $ do
      aggregatedCustomerInfo `shouldBe` Just ("Jane", "Somewhere", "some-id")

    it "should chain bind operations with a do notation" $ do
      aggregatedCustomerInfoWitDoNotation `shouldBe` Just ("Jane", "Somewhere", "some-id")

    let powersOfTwo n = do
            value <- [1 .. n]
            return (2 ^ value)

    it "should calculate the powers of two for a given number" $ do
      powersOfTwo 10 `shouldBe` [2, 4, 8, 16, 32, 64, 128, 256, 512, 1024]
