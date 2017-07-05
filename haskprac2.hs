--func that gives the abs value
lessSillyAbs :: Integer -> Integer
lessSillyAbs n = if n >= 0 then n else (-n)


--sillyAbs :: Integer -> Integer
--sillyAbs n

sillySgn :: Integer -> Integer
sillySgn n = if n == 0 then 0 else
	if n == 1 then 1 else
	if n == (-1) then (-1) else 
	if n < 0 then sillySgn (n+1) else sillySgn (n-1)
ex1 = (1,"hello") -- 2-tuple, pair
ex2 = ("hello again", 1, 5.60, True) --4-tuple
ex3 = (5, "hello again",True) -- triple

f t = (fst t + 2)
g n = (n,n+1,n+2)

fun1 x y = x+y
fun2 (x,y) = x+y

------------ example from slide 20

omit x = 0

keep_going 0 = "done"
keep_going x = keep_going (x+1)

a = map reverse ["abc", "def", "hji"]
b = map length ["abc","def","hji"]


-- mappair is our own definition for map on tuples
mappair f p = (f (fst p), f (snd p))




-- same thing but outputs as a list
mappair' f p = [f (fst p), f (snd p)]


--given a list of numbers, double every number in the list
double' x = 2*x

--twiceList map double [1,2,3,4,5]

-- map doubler [3,(-5),10,0] will double every element of the list
-- question: how do you define this without the use of map? (hint: recursion) -____-





