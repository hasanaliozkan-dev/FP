import Prelude

myFactorialL :: Int -> Int
myFactorialL n = product [1..n] 

myFactorialR :: Int -> Int
myFactorialR n = 
    if n == 0 then 1
    else n * myFactorialR (n-1)

--It doesn't work
myFactorialRR :: Int -> Int
myFactorialRR 0 = 1
myFactorialRR n = myFactorialRR (n) * myFactorialRR (n-1)

myProduct ::  Num a => [a] -> a
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs 
 
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x] 

myZip :: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys


myAnd :: [Bool] -> Bool
myAnd [] = True 
myAnd (x:xs) = x && myAnd xs 

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs

myReplicate :: Int -> a -> [a]
myReplicate 0 _  = []
myReplicate n l  = [l] ++ myReplicate(n-1) l

--Çalışmadı
{-(!!) :: [a] -> Int -> a
(x:_) !! 0 = x
(_:xs) !! n = xs!!(n-1) 
-}
myInsert :: Int -> [Int] -> [Int]
myInsert x [] = [x]
myInsert x (y:ys) = if x <= y then
                        x:y:ys
                    else 
                        y: myInsert x ys

myInsertionSort :: [Int] -> [Int]
myInsertionSort [] = []
myInsertionSort (x:xs) = myInsert x (myInsertionSort xs)

myMerge :: [Int] -> [Int] -> [Int]
myMerge [] ys =  ys
myMerge xs [] = xs
myMerge (x:xs) (y:ys) =  if x<= y then
                         x : myMerge xs (y:ys)
                         else  
                         y : myMerge (x:xs) ys

halve :: [a] -> ([a], [a]) 
halve xs = 
    ((take s xs), (drop s xs))
    where
        s = (length xs ) `div` 2

myMergeSort :: [Int] -> [Int]
myMergeSort [] = []
myMergeSort [x] = [x]
myMergeSort xs = myMerge (myMergeSort ys) (myMergeSort zs)
                where (ys,zs) = halve xs

