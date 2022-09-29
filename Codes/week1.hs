{-[x| x<- [0..10] , even x]
[x^2| x<- [1..5]]-}

{-# LANGUAGE GADTs #-}

import Prelude

data List a where
    Nil :: List a
    Cons :: a -> List a -> List a

printListH :: Show a => List a -> String
printListH l =
    case l of 
        Nil -> ""
        Cons x Nil -> show x
        Cons x xs -> show x ++ ", " ++printListH xs  

printList :: Show a => List a -> String
printList l = "[" ++ printListH l ++ "]"

myConcat :: List a -> List a -> List a
myConcat l1 l2 = 
    case l1 of
        Nil -> l2
        Cons x xs -> Cons x (myConcat xs l2) 


instance (Show a => Show (List a)) where
    show l = printListH l


data Pair a b where
    Pair :: a -> b -> Pair a b

printPair :: (Show a , Show b) => Pair a b -> String
printPair p =
    case p of
        Pair x y -> "(" ++ show x ++ "," ++ show y ++ ")"


-- add here instance show for pair

add :: x y -> 



