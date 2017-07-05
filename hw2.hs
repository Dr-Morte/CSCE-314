- HOMEWORK 2 | RICHARD PADILLA | 2/1/17

import Data.Char
import Data.List
import Data.Maybe

-- #1. Given a list, reverse it. (recursive)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse ls = last ls : myReverse ( take (length ls - 1) ls)


-- #2. judge if a value is an element of a list
isElement _ [] = False
isElement e  ls     | (head ls) == e = True
                    | otherwise      = isElement e (tail ls)


-- #3. duplicate the elements of a list. (recursive)
duplicate :: [a] -> [a]
duplicate [] = []
duplicate ls = head ls : head ls : duplicate ( tail ls)

-- #4.remove duplicate elements of a list (recursive)

removeDuplicate :: Eq a => [a] -> [a]
removeDuplicate [] = []
removeDuplicate ls = head ls : removeDuplicate (nix (head ls) (tail ls))

nix :: Eq a => a -> [a] -> [a]
nix _ [] = []
nix x xs | head xs == x = nix x (tail xs)
         | otherwise = (head xs) : nix x (tail xs)


-- #5. rotate a list n places to the left, where n is an integer. 
listRotate :: Int -> [a] -> [a]
listRotate n ls = drop n ls ++ take n ls

-- #6. Flatten a list of lists into a single list formed by concatenation. (Recursive)
flatten :: [[a]] -> [a]
flatten [] = []
flatten ls = head ls ++ flatten (tail ls)


-- #7. Given a list, determine if it is a palindrome. 
--     Note: This function IS case sensitive
isPalindrome :: Eq a => [a] -> Bool
isPalindrome ls | ls == reverse ls = True
                | otherwise        = False

-- #8. Determine weather two numbers are coprime or not. (Recursive)
coprime :: Int -> Int -> Bool
coprime a b | euclid a b == 1 = True
            | otherwise = False


euclid :: Int -> Int -> Int
euclid a b | a == 0 = b
           | b == 0 = a
           | otherwise = euclid b $ a `rem` b

 
-- (Easy) aaah!
-- seeDoctor is a very basic solution of the problem that doesnt account for lowercase and single h's.
seeDoctor :: [char] -> [char] -> Bool
seeDoctor ds ps | (length ds) <= (length ps) = True
                | otherwise = False

lowerList :: [Char] -> Bool
lowerList [] = True
lowerList ls | (isLower (head ls) == True) = lowerList (tail ls)
             | otherwise = False

countChar :: Char -> [Char] -> Int
countChar _ [] = 0
countChar c s = if c == (head s) then 1+countChar c (tail s) else countChar c (tail s)

--SeeDoctor' is the full solution that accounts for both strings being lowercase and both strings only containing 1 h. It is a very messy nested if/then/else statement.
seeDoctor' :: [Char] -> [Char]-> Bool
seeDoctor' ds ps = if lowerList ds == True then if lowerList ps == True then if (countChar 'h' ds) == 1 then if (countChar 'h' ps) == 1 then if length ds <= length ps then True else False else False else False else False else False


-- (Medium) Water Gates
data Gate = Open | Closed
	deriving (Show,Eq)

isOpen :: Gate -> Bool
isOpen g | g == Open = True
	 | otherwise = False

swapGate :: Gate -> Gate
swapGate g | (isOpen g) == True = Closed
	   | otherwise = Open

numtoGates :: Int -> [Gate]
numtoGates 0 = []
numtoGates n = (Closed : numtoGates (n-1))

numOpen :: [Gate] -> Int
numOpen [] = 0
numOpen (x:xs) | x == Open = 1 + numOpen xs
	       | otherwise = numOpen xs

-- gateChange was supposed to be the function that takes care of all the changes over n days
gateChange :: Int -> [Gate] -> [Gate]
gateChange _ [] = []
gateChange 0 xs = xs
gateChange 1 (x:xs) = (swapGate x : gateChange 1 xs)
gateChange 2 (x:xs) = --daytwo changes
gateChange n (x:xs) = (swapGate x : gateChange n xs)

-- dayChange is supposed to cycle through n days and allow gateChange to change days
dayChange :: Int -> [Gate] -> [Gate]
dayChange _ [] = []
dayChange 0 xs = xs
dayChange i xs = gateChange i xs

waterGate :: Int -> Int
waterGate n = numOpen (gateChange (numtoGates n))




-- (Hard) Goldbach's Other Conjecture
goldbachNum :: Int
goldbachNum = 17
-- not sure how I'm supposed to even find the number

-- compNum = [x | x <- [1..],x -- Need a list comp for composite numbers, and one for odd numbers

-- also need a function that checks the goldbach conjecture on a given number


-- then cycle through the odd composite numbers from 1 to infinity and return the first one that is False on goldbach's conjecture





