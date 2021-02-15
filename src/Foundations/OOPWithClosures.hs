module Foundations.OOPWithClosures where
--------------------------
-- CUP with milliliters --
--------------------------

-- constructor
-- returns a lambda that expects another function as an argument, the action/message;
-- ml argument is captured inside the lambda creating the closure
cupOf :: Int -> ((Int -> b) -> b)
cupOf ml = \actionFn -> actionFn ml

-- accessor
getMl :: ((Int -> Int) -> Int) -> Int
getMl aCup = aCup get where get = (\ml -> ml)

--modify state
add :: Int -> ((Int -> Int) -> Int) -> ((Int -> b) -> b)
add ml aCup = cupOf ( total + ml )
    where total = getMl aCup

drink :: Int -> ((Int -> Int) -> Int) -> ((Int -> b) -> b)
drink ml aCup = if totalAfterASip < 0
                then cupOf 0
                else cupOf totalAfterASip
     where  totalAfterASip = (getMl aCup) - ml

---------------------
-- Fighting Robots --
---------------------

robot :: (String, Int, Int) -> (((String, Int, Int) -> b) -> b)
robot (name, attack, hp) = \actionFn -> actionFn (name, attack, hp)

-- helpers functions
name :: (String, Int, Int) -> String
name (n, _, _) = n
attack (_, a, _) = a
hp (_, _, hp) = hp

-- accessors
getName :: (((String, Int, Int) -> String) -> String) -> String
getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHp aRobot = aRobot hp

-- modifiers
setName newName aRobot = aRobot (\(name, attack, hp) -> robot (newName, attack, hp))
setAttack newAttack aRobot = aRobot (\(name, attack, hp) -> robot (name, newAttack, hp))
setHp newHp aRobot = aRobot (\(name, attack, hp) -> robot (name, attack, newHp))

printRobot aRobot = aRobot print
    where print = (\(name, attack, hp) -> "robot(name:" ++name++ ", attack:" ++(show attack)++ ", hp:" ++ (show hp) ++")")

damage aRobot attackDamage = aRobot (\(name, attack, hp) -> robot (name, attack, hp - attackDamage))

fight attacker defender = damage defender attack
    where attack = getAttack attacker