--Just a fun daily programming exercise
--Learning how to do list processing in haskell and how to use Vim
--Done by Richard Padilla III on 1/24/17
mashList :: [a] -> [a] -> [a]
mashList xs ys = xs ++ ys

getLast :: [a] -> a
getLast xs = last xs

noCab :: [a] -> [a]
noCab xs = init xs

getInd :: Int -> [a] -> a
getInd x xs = xs !! x

listMash :: [[a]] -> [a]
listMash xs = concat xs
