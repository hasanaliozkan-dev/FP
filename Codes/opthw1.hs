import Prelude
{-
myRange :: Int -> Int -> [Int]
myRange n m = 
   if n > m then []
    else n : myRange (n+1) m-}

myDrop :: Int -> [a] -> [a]
myDrop n l = 
    case l of 
        [] -> []
        x:xs -> if n < 1 then x:xs else myDrop (n-1) xs 


myTake :: Int -> [a] -> [a]
myTake n l = 
    case l of 
        [] -> []
        x:xs -> if n < 1 then x:xs else x: myTake (n-1) xs 


myProd :: Num a => [a] -> a
myProd l = 
    case l of
        [] -> 1
        x:xs ->  x * myProd xs



