module ProgrammingInTypes.TimeSeriesWithTypesSpec where

import Test.Hspec
import ProgrammingInTypes.TimeSeriesWithTypes

spec :: Spec
spec = do
  describe "Time series" $ do

    describe "Create" $ do
      it "should create a time series" $ do
        let times = [1,4]
        let values = ["pear", "apple"]
        (createTimeSeries times values) `shouldBe` (TimeSeries [1,2,3,4] [Just "pear", Nothing, Nothing, Just "apple"])

      it "should show a time series" $ do
        let ts = (TimeSeries [1,2,3] [Just "pear", Nothing, Just "apple"])
        show ts `shouldBe` "1|\"pear\"\n2|NA\n3|\"apple\"\n"

    describe "Combine" $ do

      let ts1 = (TimeSeries [1,2] [Just "pear", Nothing, Just "apple"])
      let ts2 = (TimeSeries [2,4] [Just "pineapple", Just "orange"])
      let ts3 = (TimeSeries [5] [Just "watermelon"])
      let emptyTs = (TimeSeries [] [])

      it "should combine two time series when one is empty" $ do
        emptyTs <> ts1 `shouldBe` ts1
        ts1 <> emptyTs `shouldBe` ts1

      it "should combine two time series" $ do
        ts1 <> ts2 `shouldBe` (TimeSeries [1,2,3,4] [Just "pear", Just "pineapple", Nothing, Just "orange"])

      it "should combine an array of time series" $ do
        mconcat [ts1, ts2, ts3] `shouldBe` (TimeSeries [1,2,3,4,5] [Just "pear", Just "pineapple", Nothing, Just "orange", Just "watermelon"])

    describe "Calculations" $ do

      it "should calculate the mean" $ do
        meanTimeSeries (TimeSeries [1,2,3] [Just 3.0, Just 4.0, Just 5.0]) `shouldBe` Just 4.0

      it "should calculate the max" $ do
        maxTimeSeries (TimeSeries [1,2,3] [Just 3.0, Just 4.0, Just 5.0]) `shouldBe` Just (3, Just 5.0)

      it "should calculate the min" $ do
        minTimeSeries (TimeSeries [1,2,3] [Just 3.0, Just 4.0, Just 5.0]) `shouldBe` Just (1, Just 3.0)


