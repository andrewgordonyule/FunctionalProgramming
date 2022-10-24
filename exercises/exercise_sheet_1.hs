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
g2 y = if y == True then "you can buy beer" else "you can't buy beer"

-- 5. Write a function which applies the input function two times to the input integer
twice :: (Int -> Int) -> Int -> Int
twice func n = func (func n)

-- helper function
double :: Int -> Int
double x = x * 2

-- 6. Write a function to calculate the force between two masses m1 and m2 at distance d
force :: Float -> Float -> Float -> Float
force m1 m2 d = (g * m1 * m2) / (d ^ 2)
    where 
        g = 6.67 * (10 ^^ (-11))

-- List Comprehensions

-- 1. Using a list comprehension, calculate the numbers between 1 and 30 which are divisible by 3 (mod:: Int -> Int -> Int)
divby3 :: [Int]
divby3 = [x | x <- [1..30], x `mod` 3 == 0]

-- 2. Using a list comprehension, write a function which calculates the first n triangle numbers
triangles :: Int -> [Int]
triangles n = [ y | x <- [0..n], y <- [(x * (x + 1)) `div` 2]]

-- 3. Prime numbers using trial division

primes :: Int -> [Int]
primes n = [ x | x <- [1..n], isPrime x]
    where
        divisors x = [ y | y <- [2..(x-1)], x `mod` y == 0]
        isPrime x = length (divisors x) == 0


-- 4. Using a list comprehension, write a function which flattens a nested list
flatten :: [[a]] -> [a]
flatten xs = [ y | x <- xs, y <- x ]


-- Lecture Examples

-- pythagTriples :: Int -> [[Int]]
pythagTriples n = [ [x,y,z] | x <- [1..n], y <- [1..n], z <- [1..n], (x ^ 2) + (y ^ 2) == z ^ 2]



main = do

    -- Functions

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

    -- 5
    print("twice")
    print(twice double 2)

    -- 6
    print("force")
    print(force 1.2 1.2 5)

    -- Lists

    --1
    print("divisible by 3")
    print(divby3)

    -- 2
    print("triangles")
    print(triangles 5)

    -- 3
    print("prime numbers")
    print(primes 50)

    -- 4
    print("flatten")
    print(flatten [[1,2,3],[4,5,6]])


    --Extra example
    print("pythagorean triple")
    print(pythagTriples 20)