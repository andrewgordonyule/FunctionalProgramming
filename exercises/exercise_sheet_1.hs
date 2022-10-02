-- Exercise Sheet 1

-- Functions

-- 1. Write a function which takes two arguments as its arguments and returns the maximum value
max2 :: Int -> Int -> Int
max2 x y = if x > y then x else y
-- max2 5 10 --> 10

-- 2. Using your max2 function, write a function which returns the maximum of three numbers
max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 x (max2 y z)
-- max3 1 2 3 -> 3

-- 3. Write a function f where the resulting function is the composition of the two input functions
f :: (Int -> String) -> (String -> Bool) -> (Int -> Bool)
f f1 f2 = f2 . f1

f1 x = if x >= 18 then "you can buy beer" else "you can't buy beer"
f2 y = if y == "you can buy beer" then True else False
-- f f1 f2 18 --> True

-- 4. Write a function g where the resulting function is the output of the second input function
g :: (Int -> Bool) ->(Bool -> String) -> Int -> String
g g1 g2 num = g2 (g1 num)

g1 x = if x >= 18 then True else False
g2 y = if y == True then "you can buy beer" else "you can'y buy beer"
-- g g1 g2 18 --> "you can buy beer"