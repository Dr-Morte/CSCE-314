import Data.Char


-- #1
increaseTen :: Num a => a -> a
increaseTen x = x + 10

-- #2
circleArea :: Fractional a => a -> a
circleArea x = 3.14159 * x * x

-- #3
midList :: [a] -> [a]
midList xs = reverse (tail( reverse (tail xs)))

-- #4
countdownList ::(Enum a,Num a) => a -> a -> [a]
countdownList x y = reverse [x..y]
-- (need to recursively build a list from y to x) might need a helper funciton

-- #5
isRightTri :: (Eq a,Num a) => a -> a -> a -> Bool
isRightTri a b c = if (a*a)+(b*b)==(c*c) then True else if (b*b)+(c*c)==(a*a) then True else if (c*c)+(a*a)==(b*b) then True else False

-- #6
imgCalc :: Num a => (a,a) -> (a,a) -> (a,a)
imgCalc x y = (((fst x)*(fst y))-((snd x)*(snd y)),((fst x)*(snd y))+((fst y)*(snd x)))

-- #7. countChar: given a character and a string, count how many times that character appears in the string (recursive)

countChar :: Char -> [Char] -> Int
countChar _ [] = 0 -- we can use the underscore wildcard here because we don't care what it is, if the string is empty, it will return 0. This makes the case more general :0
countChar c s = if c == (head s) then 1+countChar c (tail s)

				      else countChar c (tail s)
-- #8
getFirsts xs = map  fst xs

-- #9. halfList: create a new list that contains every other element of an input list (i.e., retain the first element then drop the next, retain, drop, etc.) (recursive)

halfList :: [a] -> [a]
halfList [] = []
halfList ls = (head ls) : halfList' (tail ls)

halfList' [] = []
halfList' ls = halfList (tail ls)

-- #10

charCheck :: Char -> (Bool,Bool,Bool)
charCheck x | isNumber x = (False,False,True)
                | isUpper x = (True,False,False)
                | isLower x = (False,True,False)
                | otherwise = (False,False,False)

uppercaseList :: String -> [(Bool,Bool,Bool)]
uppercaseList [] = []
uppercaseList (x:xs) = (charCheck x:uppercaseList xs)

-- bonus question: write a function that computes an alternating series

altSeries :: [Int] -> Int
altSeries [] = 0
altSeries ls = (head ls) - altSeries' (tail ls)

altSeries' [] = 0
altSeries' ls = (head ls) - altSeries (tail ls)

-- THIS IS AN ALTERNATE SOLUTION TO #10 USING A 'TOGGLE VARIABLE' TO KEEP TRACK OF THE CHANGE IN STATE
altS ls = altSHelper (-1) ls

altSHelper initMult ls = (head ls) + initMult*(altSHelper (-(initMult)) (tail ls))


