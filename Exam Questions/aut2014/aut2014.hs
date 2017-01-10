-- Question 2
_sort :: (a -> a -> Bool) -> [a] -> [a]
_sort f (x1:[]) = [x1]
_sort f (x1:x2:xs) | f x1 x2 = x2:(_sort f (x1:xs))
                   | otherwise =  x1:(_sort f (x2:xs))

sort :: Eq a => (a -> a -> Bool) -> [a] -> [a]
sort f xs = let t = _sort f xs
    in if t == xs then xs else sort f t
    
    
-- Question 5
-- Scheme Function takes alternating elements from two lists putting them through a function.
foo :: (a -> b) -> [a] -> [a] -> [b]
foo f [] ys = []
foo f (x:xs) ys = (f x):foo f ys xs
