import GHC.CmmToAsm.AArch64.Instr (x0)
-- Exercise Sheet 3

-- Recursion

-- 1. Write a function which returns True if the given list contains elements in ascending order
isAscending :: [Int] -> Bool
isAscending [] = True
isAscending [x] = True
isAscending (x:y:ys) =  x < y && (isAscending (y:ys))

-- 2. Write a function which returns the given number of elements from a list
myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake n (x:xs) =
    if n <= 0 then
        []
    else
        x : (myTake (n-1) (xs))

-- 3 Write a function which returns a list without the elements in odd positions (0-indexed list)
dropOdds :: [a] -> [a]
dropOdds = dropOdds' True
    where
        dropOdds' _ [] = []
        dropOdds' True (x : xs) = x : (dropOdds' False xs) 
        dropOdds' False (_ : xs) = (dropOdds' True xs) -- ignores 1st element


-- 4. Write a function which returns a list with the given element inserted at every other position
myIntersperse :: [a] -> a -> [a]
myIntersperse [] _ = []
myIntersperse [x] y = [x, y]
myIntersperse (x : y : xs) z = x : z : myIntersperse (y:xs) z 

-- 5. Write a function which reverses the given list using recursion
myReverseRec :: [a] -> [a]
myReverseRec [] = []
myReverseRec [x] = [x]
myReverseRec (x:xs) = myReverseRec xs ++ [x]

-- 6. Write a function which reverses the given list using fold
myReverseFold :: [a] -> [a]
myReverseFold [] = []
-- 


-- Higher Order Functions

-- 1. Write a function which applies the given function to each element of a list
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f1 (x:xs) = f1 x : myMap f1 xs

f1 x = x*2


-- Algebraic Datatypes

-- 1. Suppose we have the following data type:

data ArithExpr = 
    Add ArithExpr ArithExpr
    | Sub ArithExpr ArithExpr
    | Mul ArithExpr ArithExpr
    | Div ArithExpr ArithExpr
    | Value Int

-- a) Write the function evalExpr :: ArithExpr -> Int which evaluates an expression
evalExpr :: ArithExpr -> Int
evalExpr (Add val1 val2) = v1 + v2
    where
        v1 = evalExpr val1
        v2 = evalExpr val2
evalExpr (Mul val1 val2) = 
    let v1 = evalExpr val1 in
    let v2 = evalExpr val2 in
    v1 * v2
evalExpr (Sub val1 val2) = v1 - v2
    where
        v1 = evalExpr val1
        v2 = evalExpr val2
evalExpr (Div val1 val2) = div v1 v2
    where
        v1 = evalExpr val1
        v2 = evalExpr val2
evalExpr (Value val1) = val1

-- b) Write a function showExpr :: ArithExpr -> String which prints an expression as a string 
-- e.g. showExpr Add (Mul (Value 2) (Value 4)) (Value 5) = "(2 * 4) + 5"
-- showExpr :: ArithExpr -> String
 


-- Further Exercises

main = do
    -- Recursion
    -- 1
    print "isAscending"
    print(isAscending[1,2,3])
    print(isAscending[3,2,1])

    -- 2
    print "myTake"
    print(myTake 3 [1,2,3,4,5])

    -- 3
    print "dropOdds"
    print(dropOdds [1,2,3,4,5,6,7,8,9])

    -- 4
    print "myIntersperse"
    print(myIntersperse [1,2,3] 10)

    -- 5
    print "myReverseRec"
    print (myReverseRec[1,2,3,4])

    -- Higher Order Functions
    -- 1
    print "myMap"
    print(myMap f1 [1,2,3])

    -- Algebraic Datatypes
    -- 1 a)
    print "evalExpr"
    print (evalExpr (Add (Value 2) (Value 3)))

    -- 1 b)
    print "showExpr"