module ProgrammingInTypes.TimeSeriesWithTypes where

import Data.List
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import Data.Semigroup
import Data.Maybe

type Time = Int -- just to simplify, a date is a relative index, could be months, days or millis

data TimeSeries a = TimeSeries [Time] [Maybe a] deriving Eq

--------------
-- creation --
--------------

createTimeSeries :: [Time] -> [a] -> TimeSeries a
createTimeSeries times values = TimeSeries completedTimes extendedValues
    where completedTimes = [minimum times .. maximum times] -- fill the gaps
          timeValueMap = Map.fromList (zip times values)
          extendedValues = map (\v -> Map.lookup v timeValueMap) completedTimes -- fill with Maybes

showTimeValuePair :: Show a => Time -> (Maybe a) -> String
showTimeValuePair time (Just value) = Monoid.mconcat [show time, "|", show value, "\n"]
showTimeValuePair time Nothing = Monoid.mconcat [show time, "|NA\n"]

instance Show a => Show (TimeSeries a) where
    show (TimeSeries times values) = Monoid.mconcat rows
        where rows = zipWith showTimeValuePair times values

-------------
-- combine --
-------------

combineTimeSeries :: TimeSeries a -> TimeSeries a -> TimeSeries a
combineTimeSeries (TimeSeries [] []) ts2 = ts2
combineTimeSeries ts1 (TimeSeries [] []) = ts1
combineTimeSeries (TimeSeries times1 values1) (TimeSeries times2 values2) = TimeSeries completedTimes combinedValues
    where bothTimes = Monoid.mconcat [times1, times2]
          completedTimes = [minimum bothTimes .. maximum bothTimes]
          insertMaybePairOfTimeValue = (\myMap (t,v) -> case (t,v) of
                                                             (_, Nothing) -> myMap;
                                                             (k,Just v) -> Map.insert k v myMap)
          timeValuesMap = foldl insertMaybePairOfTimeValue Map.empty (zip times1 values1)
          updatedMap = foldl insertMaybePairOfTimeValue timeValuesMap (zip times2 values2)
          combinedValues = map (\v -> Map.lookup v updatedMap) completedTimes

instance Semigroup (TimeSeries a) where
  (<>) = combineTimeSeries

instance Monoid (TimeSeries a) where
  mempty = TimeSeries [] []
  mappend = (<>)

------------------
-- calculations --
------------------

mean :: (Real a) => [a] -> Double
mean xs = total/count
    where total = (realToFrac . sum) xs
          count = (realToFrac . length) xs

meanTimeSeries :: (Real a) => TimeSeries a -> Maybe Double
meanTimeSeries (TimeSeries _ []) = Nothing
meanTimeSeries (TimeSeries times values) = if all ( == Nothing ) values
                                           then Nothing
                                           else Just avg
    where justValues = filter isJust values
          cleanValues = map fromJust justValues
          avg = mean cleanValues

type CompareFn a = a -> a -> a
type CompareFnForTimeSeries a = (Time, Maybe a) -> (Time, Maybe a) -> (Time, Maybe a)

makeCompareFnForTimeSeries :: Eq a => CompareFn a -> CompareFnForTimeSeries a
makeCompareFnForTimeSeries fn = newFunc
    where newFunc (time, Nothing) (_, Nothing) = (time, Nothing) -- pattern matching inside where
          newFunc (_, Nothing) (time, just) = (time, just)
          newFunc (time, just) (_, Nothing) = (time, just)
          newFunc (time1, Just val1) (time2, Just val2) = if fn val1 val2  == val1
                                                          then (time1, Just val1)
                                                          else (time2, Just val2)

compareTimeSeries :: Eq a => CompareFn a -> TimeSeries a -> Maybe (Time, Maybe a)
compareTimeSeries fn (TimeSeries [] []) = Nothing
compareTimeSeries fn (TimeSeries times values) = if all ( == Nothing ) values
                                           then Nothing
                                           else Just theBest
    where pairs = zip times values
          theBest = foldl (makeCompareFnForTimeSeries fn) (0, Nothing) pairs

minTimeSeries :: Ord a => TimeSeries a -> Maybe (Time, Maybe a)
minTimeSeries = compareTimeSeries min

maxTimeSeries :: Ord a => TimeSeries a -> Maybe (Time, Maybe a)
maxTimeSeries = compareTimeSeries max
