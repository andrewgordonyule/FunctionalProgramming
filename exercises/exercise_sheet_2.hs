-- Exercise Sheet 2

import Data.Char
import Data.Foldable

-- Booleans, Comparisons, Tuples and More Lists

-- 1. Write a function which returns true if the four arguments are given in ascending order
isAscending4 :: Int -> Int -> Int -> Int -> Bool
isAscending4 x1 x2 x3 x4 = x1 < x2 && x2 < x3 && x3 < x4

-- 2. Write a function swap :: (a,b) -> (b,a)
swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

-- 3. Write a function rotate :: (a, b, c) -> (c, a, b)
rotate :: (a, b, c) -> (c, a, b)
rotate (a, b, c) = (c, a, b)

-- 4. Write a function myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f a b = f (a, b)

-- 5. Write a function myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f (a, b) = f a b

-- 6. Write a function myFlip :: (a -> b -> c) -> b -> a -> c
myFlip :: (a -> b -> c) -> b -> a -> c
myFlip func x y = func y x

-- 7. Write a function mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f1 f2 (a, b) = (f1 a, f2 b)  

f1 x = 
    if x == "Hello"
        then "General"
    else "Star"

f2 x = 
    if x == "There"
        then "Kenobi"
    else "Wars"

-- 8. Write a function mapPair2 :: ((a, b) -> (c, d)) -> (a, b) -> (c, d)
mapPair2 :: ((a, b) -> (c, d)) -> (a, b) -> (c, d)
mapPair2 f3 (a,b) = f3 (a,b)

f3 (a,b) = (a+1, b+1)

-- 9. Write a function  f :: (a, b) -> (a -> c) -> (b -> d) -> ((c, d) -> e) -> e
f :: (a, b) -> (a -> c) -> (b -> d) -> ((c, d) -> e) -> e
f (a,b) func1 func2 func3 = func3 (func1 (a), func2 (b))

func1 x = x+1
func2 x = x+1
func3 (x,y) = x*y

-- IO

-- 1. Write a function which reads in a line of text, capitalises it, then prints it to the console.
echoCaps :: IO ()
echoCaps = do
    putStrLn "Enter a lower case word: "
    myText <- getLine
    putStrLn [ toUpper x | x <- myText]

-- 2. Write a function echoFile :: FilePath -> IO () which prints out a file to the console, line-by-line

echoFile :: FilePath -> IO ()
echoFile path = do
    str <- readFile path
    let strLines = lines str
    forM_ strLines (putStrLn)

-- 3. Write a function calculator :: IO () which:
-- Gets an operation (one of +, −, ∗) from the command line; if the input does not match one of these then default to (+)
-- Gets two numbers using two calls to getLin
-- Outputs the operation applied to the two integers

getOperator :: String -> (Int -> Int -> Int)
getOperator "+" = (+)
getOperator "-" = (-)
getOperator "*" = (*)
getOperator _ = (+)



calculator :: IO ()
calculator = do
    putStrLn "Enter an operator: + , - or *"
    myOperator <- getLine
    let myOperatorFunc = getOperator myOperator
    putStrLn "Enter first number: "
    num1 <- getLine
    putStrLn "Enter second number: "
    num2 <- getLine
    let result = myOperatorFunc (read num1 :: Int) (read num2 :: Int) 
    putStrLn (num1 ++ " " ++ myOperator ++ " " ++ num2 ++ " = ")
    putStrLn $ show result


main = do

    -- Booleans, Comparisons, Tuples and More Lists
    -- 1
    print("Ascending4")
    print(isAscending4 1 2 3 4)
    print(isAscending4 1 2 8 4)

    -- 2
    print("swap")
    print(swap (1,2))

    -- 3
    print("rotate")
    print(rotate (1,2,3))

    -- 4
    print("myCurry")
    print(myCurry swap 1 2)

    -- 5
    print("myUncurry")
    print(myUncurry (+) (1, 2))

    -- 6
    print("myFlip")
    print(myFlip (*) 5 6)
    print(myFlip (*) 6 5)

    print("mapPair")
    print(mapPair f1 f2 ("Hello", "There"))

    print("mapPair2")
    print(mapPair2 f3 (1,2))

    print("f")
    print(f (2,2) func1 func2 func3)

    -- IO

    print("echoCaps")
    echoCaps

    print("echoFile")
    echoFile "C:/Users/andre/Desktop/hello.txt"

