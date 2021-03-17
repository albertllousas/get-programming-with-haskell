module WorkingWithTypeInAContext.ApplicativeTypeClassSpec where

import Test.Hspec
import WorkingWithTypeInAContext.FunctorTypeClass
import qualified WorkingWithTypeInAContext.FunctorTypeClass
import WorkingWithTypeInAContext.ApplicativeTypeClass
import qualified WorkingWithTypeInAContext.ApplicativeTypeClass
import Prelude hiding (fmap, (<$>), (<*>), sequenceA, traverse, pure)

spec :: Spec
spec = do
  describe "MyApplicative" $ do

    describe "Deal with multiple values in a context (generalizing Functor's fmap to work with several arguments)" $ do

      it "should apply a function for multiple Maybe values" $ do
        let just1 = Just 1
        let just2 = Just 2
        let add = (+)
        add <$> just1 <*> just2 `shouldBe` Just 3
        pure add <*> just1 <*> just2 `shouldBe` Just 3
        app (fmap add just1) just2 `shouldBe` Just 3

      it "should concat Maybe 2 strings" $ do
        (++) <$> Just "learn" <*> Just " Haskell" `shouldBe` Just "learn Haskell"
        pure (++) <*> Just "learn" <*> Just " Haskell" `shouldBe` Just "learn Haskell"

      it "should add two Maybe values" $ do
        addMaybes (Just 2) (Just 3) `shouldBe` Just 5

      it "should increment a Maybe" $ do
        incMaybe (Just 2) `shouldBe` Just 3

      it "should be able to use a simple multi argument function using <$> and <*>" $ do
         let minOfThree x y z = min x ( min y z)
         minOfThree <$> Just 10 <*> Just 3 <*> Just 6 `shouldBe` Just 3

      it "should create a User using constructor as a function" $ do
        createUser (Just "Jane") (Just 101) (Just 10) `shouldBe` Just (User{ userName = "Jane", gamerId = 101, score = 10 })

      it "should apply a binary function to values in lists" $ do
        pure (+) <*> [1000,2000,3000] <*> [500,20000] `shouldBe` [1500,21000, 2500, 22000, 3500, 23000]

    describe "MyTraversable" $ do

      it "should flip a list of maybe" $ do
        sequenceA [Just 1, Just 2] `shouldBe` Just [1, 2]
        sequenceA [Just 1, Nothing] `shouldBe` Nothing

      it "should traverse a list from left to right, performing an action on each element." $ do
        let isEven = (\x -> if even x then Just x else Nothing)
        traverse isEven [2, 4] `shouldBe` Just [2, 4]
        traverse isEven [1, 4] `shouldBe` Nothing


