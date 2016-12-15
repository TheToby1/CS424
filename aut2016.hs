tr :: [[a]] -> [[a]]
tr ([]:_) = []
tr xs = map head xs: tr (map tail xs)

--tr :: [[a]] -> [[a]]
--tr xs = map (\z -> [y!!z | y<-xs]) [0..length(head xs)-1]
