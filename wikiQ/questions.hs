import Data.List
--Q1
myLast :: [a]->a
myLast [x] = x
myLast (_:xs) = myLast xs

--Q2
myButLast :: [a]->a 
myButLast [x,y] = x  
myButLast (_:xs) = myButLast xs

--Q3
elementAt :: [a]->Int->a
elementAt (x:xs) i | i==1 = x 
                   | otherwise = elementAt xs (i-1)

--Q4
myLength :: [a] -> Int
myLength [x] = 1
myLength (_:xs) = 1 + myLength xs
               
--Q5
myReverse :: [a]->[a]
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs++[x] 

--Q6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome (xs) = (reverse xs) == xs

--Q7
-- Don't Care

--Q8
--compress :: (Eq a) => [a] ->[a]
--compress (x:xs) = x: rest xs x
--rest :: (Eq a) => [a] -> a -> [a]
--rest [x] y | x == y = []
--          | otherwise = [x]
--rest (x:xs) y | x == y = rest xs y
--              | otherwise = [x] ++ rest xs x
compress :: (Eq a) => [a] -> [a]
compress (xs) = map head (group xs)


