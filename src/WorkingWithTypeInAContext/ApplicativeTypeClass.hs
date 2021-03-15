module WorkingWithTypeInAContext.ApplicativeTypeClass where

import Prelude hiding (fmap, (<$>), (<*>), sequenceA, traverse, pure)
import WorkingWithTypeInAContext.FunctorTypeClass

-- Applicative's allows to apply a function in a context
class MyFunctor f => MyApplicative f where
   pure :: a -> f a
   app :: f (a -> b) -> f a -> f b
   (<*>) :: f (a -> b) -> f a -> f b
   (<*>) = app

instance MyApplicative Maybe where
    pure = Just
    app (Just f) (Just x) = Just (f x)
    app _ _ = Nothing

-------------------------------------------------------------------
-- Dealing with multiple values in a context, generalise Functor --
-------------------------------------------------------------------

applyToMaybes :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
applyToMaybes fn x y = partiallyAppliedFn <*> y
    where partiallyAppliedFn = fn <$> x -- type :: Maybe (b -> c)

-- lift a function
lift2Maybes = applyToMaybes

addMaybes = lift2Maybes (+)

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe x = partiallyAppliedInc <*> x
    where partiallyAppliedInc = (+) <$> Just 1 -- type :: Num a => Maybe (a -> a)

data User = User { userName :: String, gamerId :: Int, score :: Int } deriving (Eq, Show)

createUser :: Maybe String -> Maybe Int -> Maybe Int -> Maybe User
createUser userName gamerId score = User <$> userName <*> gamerId <*> score


-----------------
-- Traversable --
-----------------

class (MyFunctor t) => MyTraversable t where
    traverse  :: MyApplicative f => (a -> f b) -> t a -> f (t b)
    sequenceA :: MyApplicative f => t (f a) -> f (t a)

instance MyTraversable [] where
    traverse _ []     = pure []
    traverse f (x:xs) = (:) <$> f x <*> traverse f xs
    sequenceA [] = pure []
    sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
