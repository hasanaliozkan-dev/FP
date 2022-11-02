{-# LANGUAGE GADTs #-}
--Generalised Algebric Datatypes
import Prelude

---------------------------------------------------------------- week 0 ----------------------------------------------------------------
myRange :: Int -> Int -> [Int]
myRange n m = 
    if n > m then [] else n : myRange(n+1) m

mySum :: [Int] -> Int
mySum n =
    case n of
        []   -> 0
        x:xs -> x + mySum xs
myConcat:: [Int] -> [Int] -> [Int]
myConcat n m = 
    case n of 
        [] -> m
        x:xs -> x : myConcat xs m

factorial:: Int -> Int
factorial n =   if n == 0 then error "ERRORRRR!!!!" 
                else if n < 2 then 1
                else n * (factorial (n-1))

myReverse :: [Int] -> [Int]
myReverse n =
    case n of 
        []      -> []
        x:xs    -> myConcat(myReverse xs) [x] 
myLength :: [Int] -> Int
myLength l = 
    case l of
        []      -> 0
        x:xs    -> 1 + myLength xs

qSort :: [Int] -> [Int]
qSort l = 
    case l of
        []      -> []
        x:xs    ->
            let 
                le = [a | a <-xs , a<=x]
                gt = [b | b <-xs , b>x ]
            in
                qSort le ++ [x] ++ qSort gt

---------------------------------------------------------------- week 1 ----------------------------------------------------------------

data List a where
    Nil  :: List a
    Cons :: a -> List a -> List a


