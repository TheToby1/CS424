--works, messy
--mapEveryOther :: (a->a) -> [a] -> [a]
--mapEveryOther f (x:[]) = [f x]
--mapEveryOther f (x:y:[]) = f x : [y]
--mapEveryOther f (x:y:xs) = f x : [y] : mapEveryOther f (xs)

mapEveryOther :: (a->a) -> [a] -> [a]
mapEveryOther f xs = zipWith ($) (cycle [(f),id]) xs
