f1 x = 2*x
f2 x = x^2-1
f3 x = 6+5*x

fxns = [f1,f2,f3]
nums = [1..5]

--how do i apply each of fxns to  ums?

v1 fxns nums = [f v | f <- fxns, v <- nums]

apply f nums = map f nums

v2 [] nums = []
v2 (f:fns) nums = (map f nums) ++ (v2 fns nums)

--next prob try to do the halving multiplication algorithm
-- $ is a cheap way to put parenthesis aorund something?

multipl m n = sum $ map snd ( removeEvens ( makeList m n))

makeList m n | m == 1		 = [(m,n)]
             | otherwise	 = [(m,n)] ++ makeList (m `div` 2) (n*2)

removeEvens [] = []
removeEvens ((a,b):ls) = if (even a) then removeEvens ls else (a,b): (removeEvens ls)

listHalf 1 = []
--listHalf x = if x ! 1 then (x:listHalf (x/2)) else (1:x)

--can we do this multiplication with a list comprehension?
