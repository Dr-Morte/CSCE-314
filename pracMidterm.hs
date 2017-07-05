-- This is the code for the Practice Midterm


-- #1.
compoundReturn :: (Fractional a,Num a) => a -> a -> [a]
compoundReturn f i = f : (compoundReturn' f i)

compoundReturn' ::(Fractional a, Num a) => a -> a -> [a]
compoundReturn' f i = (produceElem f i) : (compoundReturn' (produceElem f i) i)

produceElem ::(Fractional a, Num a)=> a -> a -> a
produceElem x y = (x *(y/100)) + x


-- #2. 
data Weapon = Rock | Paper | Scissors
	deriving Eq

rps :: Weapon -> Weapon -> Int
rps a b | a == Rock && b == Scissors = 1
	| a == Paper && b == Rock = 1
	| a == Scissors && b == Paper = 1
	| a == Rock && b == Paper = 2
	| a == Paper && b == Scissors = 2
	| a == Scissors && b == Rock = 2
	| otherwise = 0


rps' :: Weapon -> Weapon -> Int
rps' Rock Rock = 0
rps' Rock Paper = 2
rps' Rock Scissors = 1
rps' Paper Paper = 0
rps' Paper Scissors = 2
rps' Paper Rock = 1
rps' Scissors Scissors = 0
rps' Scissors Rock = 2
rps' Scissors Paper = 1

-- #3.

listMult :: Num a => [a] -> [a] -> a
listMult [] _ = 0
listMult _ [] = 0
listMult (x:xs)(y:ys) = x*y + listMult xs ys


weightAvg :: (Fractional a, Num a) => [a] -> [a] -> a
weightAvg xs ys =  (listMult xs ys)/(sum xs)





-- #6

-- #6.1
palindrome xs = reverse xs == xs
-- #6.2
fls = [take 12]
-- #6.3
tripler f x = f( f( f x))


-- #7. 

-- #7.1

myLister :: [[x]] -> [x]
myLister ls = [x | xs <- ls, (length xs `mod` 2 == 0), x <- xs]

-- #7.2
functionr :: (Enum t, Num t) => t -> [a] -> [[a]]
functionr x ls = [ls++ls | _ <- [1..x]]

-- #7.3
isOdd 0 = False
isOdd 1 = True
isOdd n = isEven (n-1)

isEven 0 = True
isEven 1 = False
isEven m = isOdd (m-1)
