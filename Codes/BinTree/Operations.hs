
module Operations where

import Prelude
import BinTree


size :: BinTree a -> Int
size t =
    case t of
        Empty      -> 0
        Node n l r -> 1 + size l + size r

height :: BinTree a -> Int
height t =
    case t of
        Empty      -> 0
        Node n l r -> 1 + max (height l) (height r) 

fromList :: [a] -> BinTree a
fromList l =
    case l of
        []   -> Empty
        x:xs -> Node x Empty (fromList xs)

make :: [a] -> BinTree a 
make l =
    case l of 
        []          -> Empty
        list@(x:xs) -> 
            let n          = div (length list) 2
                (ys, z:zs) = splitAt n list
            in Node z (make ys) (make zs)

insert :: Ord a => BinTree a -> a -> BinTree a
insert t x =
    case t of
        Empty      -> Node x Empty Empty
        Node n l r -> if x > n then Node n l (insert r x) else Node n (insert l x) r

searchTree :: Ord a => [a] -> BinTree a
searchTree l = foldl (insert) (Empty) l

flatten :: BinTree a -> [a]
flatten t =
    case t of
        Empty      -> []
        Node n l r -> flatten l ++ [n] ++ flatten r

sort :: Ord a => [a] -> [a]
sort l = flatten (searchTree l)
