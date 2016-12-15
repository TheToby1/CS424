import Data.List

--afterFilter :: (a -> Bool) -> [a] -> [a]
--afterFilter f [] = []
--afterFilter f (x:xs) = 
--    if null xs
--    then []
--    else
--        if f x
--        then head xs:afterFilter f xs
--        else afterFilter f xs

--afterFilter :: (a -> Bool) -> [a] -> [a]
--afterFilter f xs = 
--    let ind = map (+1) (findIndices f (init xs))
--    in zipWith (!!) (take (length ind) (repeat xs)) ind
    
afterFilter :: (a -> Bool) -> [a] -> [a]
afterFilter f [] = []
afterFilter f xs = [xs!!y | y<-map (+1) (findIndices f (init xs))]
