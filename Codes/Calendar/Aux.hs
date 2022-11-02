
module Aux where

import Prelude

myZip :: [a] -> [b] -> [(a, b)]
myZip l1 l2 =
    case l1 of
        []   -> []
        x:xs ->
            case l2 of
                []   -> []
                y:ys -> (x, y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f l1 l2 =
    case l1 of
        []   -> []
        x:xs ->
            case l2 of
                []   -> []
                y:ys -> f x y : myZipWith f xs ys

myRjustify :: Int -> String -> String
myRjustify n s = if n >= length s 
                 then replicate (n-length s) ' ' ++ s
                 else error "list too short to be ..." 

myDrop :: Int -> [a] -> [a]
myDrop n l =
    case l of
        []   -> []
        x:xs -> if n < 1 then x:xs else myDrop (n-1) xs

myTake :: Int -> [a] -> [a]
myTake n l =
    case l of
        []   -> []
        x:xs -> if n < 1 then [] else x : myTake (n-1) xs

mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt n l = (myTake n l, myDrop n l)

myGroupsOfSize :: Int -> [a] -> [[a]]
myGroupsOfSize n l =
    let (xs, ys) = mySplitAt n l 
    in if null xs
       then []
       else xs : myGroupsOfSize n ys

myFoldr1 :: (a -> a -> a) -> [a] -> a
myFoldr1 f l =
    case l of
        []   -> error "function does not work on the empty list"
        [x]  -> x
        x:xs -> f x (myFoldr1 f xs) 

myMap :: (a -> b) -> [a] -> [b]
myMap f l =
    case l of
        []   -> []
        x:xs -> f x : myMap f xs