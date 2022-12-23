import Prelude

myRange :: Int -> Int -> [Int]
myRange n m = 
    if n > m then []
    else n : myRange (n+1) m

mySum :: [Int] -> Int
mySum l =
    case l of
        []   -> 0
        x:xs -> x + mySum xs

myConcat :: [a] -> [a] -> [a]
myConcat l1 l2 =
    case l1 of
        []   -> l2
        x:xs -> x : myConcat xs l2

factorial :: Int -> Int
factorial n =
    if n < 0 then error "factorial function is only defined for non-negative integers"
    else if n < 2 then 1
    else n * (factorial (n-1))

myReverse :: [a] -> [a]
myReverse l =
    case l of
        []   -> []
        x:xs -> myConcat (myReverse xs) [x]

myLength :: [a] -> Int
myLength l =
    case l of
        []   -> 0
        x:xs -> 1 + myLength xs

qSort :: [Int]-> [Int]
qSort l = 
    case l of 
        [] -> []
        x : xs ->
            let 
                le = [a | a <- xs, a <= x] -- [a for a in xs if a <= x]
                gt = [a | a <- xs , a > x] -- [a for a in xs if a > x]
            in 
                qSort le ++ [x] ++ qSort gt