import System.Environment

grep :: [Char] -> [[Char]] -> [[Char]]
grep [] _ = []
grep _ [] = []
grep s xs = [x | x <- xs, substring s x]

substring :: [Char] -> [Char] -> Bool
substring [] _ = False
substring _ [] = False
substring s (h:t) = if length s <= length ([h] ++ t) && eql_pairs (zip s ([h] ++ t))
                      then True
                      else substring s t

eql_pairs :: [(Char,Char)] -> Bool
eql_pairs ps = length [p | p <- ps, eql_pair p] == length ps

eql_pair :: (Char,Char) -> Bool
eql_pair (a,b) = a == b

main = do
  [p, f] <- getArgs
  s      <- readFile f
  putStr (unlines (grep p (lines s)))
