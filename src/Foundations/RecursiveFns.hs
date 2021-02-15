module Foundations.RecursiveFns where

myLength :: [a] -> Int
myLength [] = 0
myLength xs = 1 + myLength ( tail xs )

myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = x:rest
    where rest = myTake (n-1) xs

myMap :: (a->b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = (f x):(myMap f xs)

myFilter :: ( a -> Bool ) -> [a] ->[a]
myFilter _ [] = []
myFilter f (x:xs) = if f x
                    then x:(myFilter f xs)
                    else myFilter f xs

-- operate on the accumulate
myFoldLeft :: (a -> b -> a) -> a -> [b] -> a
myFoldLeft _ acc [] = acc
myFoldLeft binaryFn acc (x:xs) = myFoldLeft binaryFn newAcc xs
                                 where newAcc = binaryFn acc x

-- operate on the first element
myFoldRight :: (a -> b -> b) -> b -> [a] -> b
myFoldRight _ acc [] = acc
myFoldRight binaryFn acc (x:xs) = binaryFn x rightResult
                                  where rightResult = myFoldRight binaryFn acc xs