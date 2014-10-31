substring :: [Char] -> [Char] -> Bool
substring [] _ = False
substring _ [] = False
substring xs (y:ys) = if length xs <= length ([y] ++ ys) && eql_pairs (zip xs ([y] ++ ys))
                      then True
                      else substring xs ys

eql_pairs :: Eq a => [(a,a)] -> Bool
eql_pairs xs = length [x | x <- xs, eql_pair x] == length xs

eql_pair :: Eq a => (a,a) -> Bool
eql_pair (a,b) = a == b