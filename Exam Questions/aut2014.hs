sort :: (a -> a -> Bool) -> [a] -> [a]
sort f (a:b:xs) = 
