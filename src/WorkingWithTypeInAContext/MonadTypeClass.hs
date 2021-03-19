{-# OPTIONS_GHC -XPolyKinds #-}
{-# LANGUAGE RebindableSyntax #-} -- enable do notation for non-monads
module WorkingWithTypeInAContext.MonadTypeClass where

import Prelude hiding (Maybe,Just,Nothing,(>>=), (>>), return, fail, fmap)
import ProgrammingInTypes.MaybeType
import WorkingWithTypeInAContext.FunctorTypeClass
import WorkingWithTypeInAContext.ApplicativeTypeClass

class MyApplicative m => MyMonad (m :: * -> *) where
    (>>=) :: m a -> (a -> m b) -> m b -- bind method
    (>>) :: m a -> m b -> m b
    return :: a -> m a
    fail :: String -> m a

instance MyMonad Maybe where
    (Just x) >>= fn = fn x
    Nothing >>= _ = Nothing
    (Just _) >> y = y
    Nothing >> _ = Nothing
    return x = Just x
    fail _ = Nothing

instance MyMonad [] where
    [] >>= fn = []
    xs >>= fn = concat (fmap fn xs)
    _ >> y = y
    return x = [x]
    fail _ = []


-------------------------------------------
-- Check the tests for examples of usage --
-------------------------------------------

inc :: Num a => Maybe a -> Maybe a
inc x = x >>= addOne
    where addOne = \x -> Just (x + 1)

getCustomerName = Just "Jane"
getCustomerAddress = Just "Somewhere"
getCustomerId = Just "some-id"

aggregatedCustomerInfo = getCustomerName >>=
                              ( \name -> getCustomerAddress >>=
                                 ( \address  -> getCustomerId >>=
                                    (\customerId -> return (name, address, customerId) )
                                  )
                              )

aggregatedCustomerInfoWitDoNotation = do
   name <- getCustomerName
   address <- getCustomerAddress
   customerId <- getCustomerId
   return (name, address, customerId)