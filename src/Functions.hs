module Functions where

-- lambda
addOne = \x -> (x::Int) + 1

-- simple function
inc :: Int -> Int
inc x = x + 1

-- HOF pass
ifEven apply n = if even n 
                 then apply n
                 else n

ifEvenInc n = ifEven addOne n

-- HOF return
flipBinaryArgs binaryFunction = (\x y -> binaryFunction y x)

-- pattern matching
factorialWithPatternMatching :: Int -> Int
factorialWithPatternMatching 0 = 1
factorialWithPatternMatching n = n * factorialWithPatternMatching ( n - 1 )

-- Guards
factorialWithGuards :: Int -> Int
factorialWithGuards n | n == 0 = 1
                      | n /= 0 = n * factorialWithGuards ( n - 1 )

-- Where clause
squaresWithWhereClause :: (Int, Int) -> (Int, Int)
squaresWithWhereClause (a, b) = (square a, square b)
                where square a = a * a

-- let in clause
squaresWithLetIn :: (Int, Int) -> (Int, Int)
squaresWithLetIn (a, b) = let square a = a * a
                 in (square a, square b)