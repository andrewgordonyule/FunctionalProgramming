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

main = do
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