
module Operations where

import Prelude
import BTree 

size :: BTree a -> Int
size t =
    case t of
        Empty      -> 0
        Node n l r -> 1 + size l + size r

height :: BTree a -> Int
height t =
    case t of
        Empty      -> 0
        Node n l r -> 1 + max (height l) (height r) 

fromList :: [a] -> BTree a
fromList l =
    case l of
        []   -> Empty
        x:xs -> Node x Empty (fromList xs)

make :: [a] -> BTree a 
make l =
    case l of 
        []          -> Empty
        list@(x:xs) -> 
            let n          = div (length list) 2
                (ys, z:zs) = splitAt n list
            in Node z (make ys) (make zs)

insertTree :: Ord a => BTree a -> a -> BTree a
insertTree t x =
    case t of
        Empty      -> Node x Empty Empty
        Node n l r -> if x > n 
                      then Node n l (insertTree r x) 
                      else if x == n 
                      then t
                      else Node n (insertTree l x) r

searchTree :: Ord a => [a] -> BTree a
searchTree l = foldl (insertTree) (Empty) l

flatten :: BTree a -> [a]
flatten t =
    case t of
        Empty      -> []
        Node n l r -> flatten l ++ [n] ++ flatten r

sort :: Ord a => [a] -> [a]
sort l = flatten (searchTree l)


memberTree:: Ord a => a -> BTree a -> Bool
memberTree x t = 
    case t of 
        Empty -> False
        Node n l r -> 
            case (compare x n ) of 
                EQ -> True
                LT -> memberTree x l
                GT -> memberTree x r

    
unionTree :: Ord a => BTree a -> BTree a -> BTree a   
unionTree t1 t2 = 
    case t1 of 
        Empty -> t2
        Node n l r -> insertTree(unionTree(unionTree l r ) t2) n 

splitMaxFromTree :: Ord a => BTree a -> Maybe (a, BTree a)
splitMaxFromTree t = 
            case t of 
                Empty -> Nothing  
                Node n l Empty -> Just (n , l)
                Node n l r -> let Just (m , r') = splitMaxFromTree r
                                in Just(m , Node n l r')

removeFromTree :: Ord a => a -> BTree a  -> BTree a
removeFromTree x t = 
    case t of 
        Empty -> Empty 
        Node n l r -> 
            case (compare x n) of 
                EQ -> case (splitMaxFromTree l) of 
                    Nothing -> r 
                    Just (m , l') -> Node m l' r 
                LT -> Node n (removeFromTree x l ) r
                GT -> Node n l (removeFromTree x r)

differenceTree :: Ord a => BTree  a -> BTree a -> BTree a 

differenceTree t1 t2 = t1