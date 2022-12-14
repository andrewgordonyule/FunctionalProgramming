-- FutureLearn interactive examples

-- Recursion

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength (xs)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred [] = []
myFilter pred (x:xs) = 
    if (pred x) then (x : myFilter pred (xs))
    else myFilter pred (xs)


-- Function Composition


main = do
    -- Recursion
    print("List recursion example: myLength")
    print(myLength [1,2,3,4,5])

    print("Filter example")
    print(filter (<5) [2,4,7,9])

    print("myFilter example")
    print(myFilter (<5) [2,4,7,9])

    -- Function Composition
    print("Function composition using map")
    print(map ((+5) . (*3)) [1 .. 10])
    
    print("Reverse example with foldl")
    print(foldl (\acc x -> x:acc) "" "Reverse")