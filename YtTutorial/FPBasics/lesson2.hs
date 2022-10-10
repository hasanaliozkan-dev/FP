import Prelude

myAbs :: Int -> Int
myAbs n = if n >= 0 then n else -n

--In haskell we must put the else 
--Guarded Equations  otherwise is catch all condition
myAbs2 n    | n>=0 = n
            | otherwise = -n 

(&) :: Bool -> Bool -> Bool
True & True = True
False & True = False
True & False = False
False & False = False
{-
1:(2:(3:[]))
[1,2,3]
-}

safeTail xs = if null xs then 
                []
                else tail xs

safeTailG xs   | null xs = []
                | otherwise = tail xs

safeT (_:xs) = xs



--{x^2 | x € {1,2,3,4,5}}   === [x^2 | x <- [1..5]]
--[(x,y) | x <-[1..5], y <-[1..3]]
--[(x,y) | x <-[1..3], y <-[x..3]]

myCartesian2 :: [a] -> [b] ->[(a,b)]
myCartesian2 l1 l2 =
    [(x,y) | x <- l1, y<-l2]


myCartesian3 :: [a] -> [b] -> [c] ->[(a,b,c)]
myCartesian3 l1 l2 l3 =
    [(x,y,z) | x <- l1, y<-l2, z <- l3]

myConcat :: [[a]] -> [a]
myConcat xss = 
    [x | xs <-xss, x <-xs] 

myFactor :: Int -> [Int]
myFactor n = 
    [x | x <- [1..n], mod n x == 0]

isPrime :: Int -> Bool
isPrime n = 
    myFactor n == [1,n]

primeNumbers :: Int -> [Int]
primeNumbers n = 
    [x | x <- [2..n], isPrime x]


--Faster version 
allPrimes :: [Int]
allPrimes = sieve [2..]

sieve :: [Int] -> [Int]
sieve(p:xs) = p : sieve [x | x <- xs , x `mod` p /=0] 

--Zip
myZip :: [a] -> [b] -> [(a,b)]
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys
myZip _      _      = []

--Pairs
myPairs :: [a] -> [(a,a)]
myPairs xs = myZip xs (tail xs)

isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = and [x<=y | (x,y) <- myPairs xs]

isSortedDesc :: Ord a => [a] -> Bool
isSortedDesc xs = and [x>=y | (x,y) <- myPairs xs]

positionsOfItem :: Eq a => a -> [a] -> [Int]
positionsOfItem n xs = 
    [i | (n',i) <- myZip xs [0..], n == n']


countOfItem :: Eq a => a -> [a] -> Int
countOfItem x xs = length [x' | x' <- xs , x == x']



isPyths :: Int -> Int -> Int -> Bool
isPyths n l p = n^2 + l^2 == p^2

myPyths :: Int -> [(Int,Int,Int)]
myPyths p = [(n,l,p)| (n,l) <- myCartesian2 [1..p] [1..p]  , isPyths n l p]


mySum :: [Int] -> Int
mySum l =
    case l of
        []   -> 0
        x:xs -> x + mySum xs

isPerfect :: Int -> Bool
isPerfect n = mySum(myFactor n) -n == n

myPerfects :: Int -> [Int]
myPerfects n = [n | n <- [1..n] ,isPerfect n] 

myScalar :: [Int] -> [Int] -> Int
myScalar xs ys = mySum [xs!!i * ys!!i | i <- [0..n-1]]
                where n = length xs 

myScalarWZip :: [Int] -> [Int] -> Int
myScalarWZip xs ys = mySum [x*y | (x,y)<- myZip xs ys] 


