-- Exercise Sheet 1

-- Functions

-- 1. Write a function which takes two arguments as its arguments and returns the maximum value
max2 :: Int -> Int -> Int
max2 x y = if x > y then x else y

-- 2. Using your max2 function, write a function which returns the maximum of three numbers
max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 x (max2 y z)

-- 3. Write a function f where the resulting function is the composition of the two input functions
f :: (Int -> String) -> (String -> Bool) -> (Int -> Bool)
f f1 f2 = f2 . f1
-- lecture version -> also works
-- f f1 f2 = \i -> f2 (f1 i)

f1 x = if x >= 18 then "you can buy beer" else "you can't buy beer"
f2 y = if y == "you can buy beer" then True else False

-- 4. Write a function g where the resulting function is the output of the second input function
g :: (Int -> Bool) ->(Bool -> String) -> Int -> String
g g1 g2 num = g2 (g1 num)

g1 x = if x >= 18 then True else False
g2 y = if y == True then "you can buy beer" else "you can'y buy beer"

-- 5
force :: Float -> Float -> Float -> Float
force m1 m2 d = (g * m1 * m2) / (d ^ 2)
    where g = 6.67 * (10 ^ (-11))


-- pythagTriples :: Int -> [[Int]]
-- pythagTriples n = [ [xyz] | x <- [1..n], y <- [1..n], z <- [1..n], (x ^ 2) + (y ^ 2) == z ^ 2]

primes :: Int -> [Int]
primes n = [ x | x <- [1..n], isPrime x]
    where
        divisors x = [ y | y <- [2..(x-1)], x `mod` y == 0]
        isPrime x = length (divisors x) == 0

main = do

    -- 1
    print("max2 5 10")
    print(max2 5 10)

    -- 2
    print("max3 1 2 3")
    print(max3 1 2 3)

    -- 3
    print("f f1 f2 18")
    print(f f1 f2 18)

    -- 4
    print("g g1 g2 18")
    print(g g1 g2 18)

    -- -- pythag
    -- pythagTriples 20