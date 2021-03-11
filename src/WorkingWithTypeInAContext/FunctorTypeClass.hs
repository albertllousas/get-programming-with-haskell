module WorkingWithTypeInAContext.FunctorTypeClass where

import Prelude hiding (fmap)

class MyFunctor f where -- f is a type constructor with kind * -> *
    fmap :: (a -> b) -> f a -> f b -- Note: f does not stand for function, it means functor
    (<$>) :: (a -> b) -> f a -> f b
    (<$>) = fmap

instance MyFunctor Maybe where
    fmap fn Nothing = Nothing
    fmap fn (Just x) = Just (fn x)
