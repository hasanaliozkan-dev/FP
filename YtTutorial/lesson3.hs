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


myMapL :: (a->b) -> [a] -> [b]
myMapL f xs = [f x | x<- xs]

{-myMapR :: (a->b) -> [a] -> [b]
myMapR f [] = []
myMapR f xs = f x : myMapR f xs
-}
myFilterL :: (a->Bool) -> [a] -> [a]
myFilterL p xs = [x | x <- xs , p x]

{-myFilterR ::  (a->Bool) -> [a] -> [a]
myFilterR p [] = []
myFilterR p (x:xs) =
    | p xs         = x : myFilterR p xs
    | otherwise    = myFilterR p xs
-}
myFoldr :: (a->b->b) -> b->[a]->b
myFoldr f v [] = v
myFoldr f v (x:xs) = f x (myFoldr f v xs)
 
(.) :: (b -> c) -> (a->b) -> (a->c)
f . g = \x -> f (g x)  
{-}
myOdd :: Int -> Bool
myOdd n = not (.) even n
-}
myAll :: (a-> Bool) -> [a] -> Bool
myAll p xs = and  [p x | x <- xs]

myAny :: (a-> Bool) -> [a] -> Bool
myAny p xs = or [p x | x <- xs]

myTakeWhile :: (a-> Bool) -> [a] -> [a]
myTakeWhile p [] = []
myTakeWhile p (x:xs) =
    | p x            = x : myTakeWhile p xs
    | otherwise      = []  


myDropWhile :: (a-> Bool) -> [a] -> [a]
myDropWhile p [] = []
myDropWhile p (x:xs) =
    | p x            = x : myDropWhile p xs
    | otherwise      = x:xs

-- [f x | x<-xs , p x] == map f (filter p xs) 