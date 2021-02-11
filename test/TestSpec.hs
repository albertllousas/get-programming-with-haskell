module TestSpec where

import Test.Hspec
import Control.Exception (evaluate)

spec :: Spec
spec = do
  describe "Prelude.head2" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException
