-- Exercise Sheet 2

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
    --

    -- 5
    print("myUncurry")
    -- 


    -- IO
