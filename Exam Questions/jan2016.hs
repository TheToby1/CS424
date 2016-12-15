count:: [a] -> [Int] -> [a]

count [] [] = []
count (x:xs) (y:ys) = take y(repeat x)++count xs ys

revCount:: [a] -> [Int] -> [a]
revCount xs ys = reverse $ count xs ys 
