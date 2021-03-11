module WorkingWithTypeInAContext.FunctorTypeClassSpec where

import Test.Hspec
import WorkingWithTypeInAContext.FunctorTypeClass
import qualified WorkingWithTypeInAContext.FunctorTypeClass
import Prelude hiding (fmap, (<$>))

spec :: Spec
spec = do
  describe "MyFunctor" $ do
    it "should map a maybe" $ do
       fmap (+ 1) (Just 1) `shouldBe` Just 2
       fmap (+ 1) Nothing `shouldBe` Nothing
       (+ 1) <$> (Just 1) `shouldBe` Just 2
       (+ 1) <$> Nothing `shouldBe` Nothing