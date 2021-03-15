module WorkingWithTypeInAContext.FunctorTypeClassSpec where

import Test.Hspec
import WorkingWithTypeInAContext.FunctorTypeClass
import qualified WorkingWithTypeInAContext.FunctorTypeClass
import Prelude hiding (fmap, (<$>))

spec :: Spec
spec = do
  describe "MyFunctor" $ do

    it "should map over a maybe" $ do
      fmap (+ 1) (Just 1) `shouldBe` Just 2
      fmap (+ 1) Nothing `shouldBe` Nothing
      (+ 1) <$> (Just 1) `shouldBe` Just 2
      (+ 1) <$> Nothing `shouldBe` Nothing

    it "should map over an array" $ do
      fmap (+ 1) [1,2,3] `shouldBe` [2, 3, 4]
      (+ 1) <$> [1,2,3] `shouldBe` [2, 3, 4]

    it "should render robot parts to Html" $ do
      let leftArm = RobotPart { name = "left arm", cost = 1000.00, count = 3 }
      let rightArm = RobotPart { name = "right arm", cost = 1025.00, count = 5 }
      let robotHead = RobotPart { name = "head", cost = 5092.25, count = 2 }
      renderPartsToHtml [leftArm, rightArm, robotHead] `shouldBe` ["<h2>left arm</h2><p> Cost: 1000.0</p><p>Count: 3</p>","<h2>right arm</h2><p> Cost: 1025.0</p><p>Count: 5</p>","<h2>head</h2><p> Cost: 5092.25</p><p>Count: 2</p>"]