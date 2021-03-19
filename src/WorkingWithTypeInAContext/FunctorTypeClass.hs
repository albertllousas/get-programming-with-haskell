{-# OPTIONS_GHC -XPolyKinds #-}
module WorkingWithTypeInAContext.FunctorTypeClass where

import Prelude hiding (Maybe,Just,Nothing,fmap, (<$>))
import ProgrammingInTypes.MaybeType
import qualified Data.Map as Map

-- A functor represents a type that can be mapped over
class MyFunctor (f :: * -> *) where -- f is a type constructor with kind * -> *
    fmap :: (a -> b) -> f a -> f b -- Note: f does not stand for function, it means functor
    (<$>) :: (a -> b) -> f a -> f b
    (<$>) = fmap

instance MyFunctor Maybe where
    fmap fn Nothing = Nothing
    fmap fn (Just x) = Just (fn x)

instance MyFunctor [] where
    fmap fn [] = []
    fmap fn (x:xs) = (fn x): fmap fn xs

-------------------------
-- example with robots --
-------------------------

data RobotPart = RobotPart { name :: String, cost :: Double, count :: Int}

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part = mconcat ["<h2>", (name part), "</h2>", "<p> Cost: ", (show (cost part)), "</p>", "<p>Count: ", (show (count part)), "</p>"]

renderPartsToHtml :: [RobotPart] -> [Html]
renderPartsToHtml parts = renderHtml <$> parts

