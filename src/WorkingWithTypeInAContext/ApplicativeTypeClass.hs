module WorkingWithTypeInAContext.ApplicativeTypeClass where

import Prelude hiding (Maybe,Just,Nothing,fmap, (<$>), (<*>), sequenceA, traverse, pure)
import ProgrammingInTypes.MaybeType
import WorkingWithTypeInAContext.FunctorTypeClass

-- Applicative's allows to apply a function in a context
class MyFunctor f => MyApplicative f where
   pure :: a -> f a -- put an ordinary value or function in a context
   app :: f (a -> b) -> f a -> f b
   (<*>) :: f (a -> b) -> f a -> f b
   (<*>) = app

instance MyApplicative Maybe where
    pure = Just
    app (Just f) (Just x) = Just (f x)
    app _ _ = Nothing

instance MyApplicative [] where
   pure x = [x]
   app fs xs = [f x | f <- fs, x <- xs] -- List comprehension: https://wiki.haskell.org/List_comprehension

-------------------------------------------------------------------
-- Dealing with multiple values in a context, generalise Functor --
-------------------------------------------------------------------

applyToMaybes :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
applyToMaybes fn x y = partiallyAppliedFn <*> y
    where partiallyAppliedFn = fn <$> x -- put a fn in a context with type :: Maybe (b -> c)

-- lift a function
lift2Maybes = applyToMaybes

addMaybes = lift2Maybes (+)

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe x = partiallyAppliedInc <*> x
    where partiallyAppliedInc = (+) <$> Just 1 -- type :: Num a => Maybe (a -> a)

data User = User { userName :: String, gamerId :: Int, score :: Int } deriving (Eq, Show)

createUser :: Maybe String -> Maybe Int -> Maybe Int -> Maybe User
createUser userName gamerId score = User <$> userName <*> gamerId <*> score -- data constructor can be used as a fn


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
