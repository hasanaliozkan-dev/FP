{-# LANGUAGE GADTs #-}

module SetADT(Set,insert,insertList,member,union,difference,removeDups) where

import Prelude

data Set a where
    Cons :: [a] -> Set a

setADT2String :: Show a => Set a -> String
setADT2String s =
    case s of
        Cons l -> "{" ++ list2String l ++ "}"
                    where
                        list2String l =
                             case l of
                                []   -> ""
                                [e]  -> show e
                                x:xs -> show x ++ "," ++ list2String xs

instance Show a => Show (Set a) where
    show s = setADT2String s

empty :: Set a
empty = Cons []

isIn :: Eq a => a -> [a] -> Bool
isIn e l =
    case l of
        []   -> False
        x:xs -> if x == e then True else isIn e xs

removeDupsH :: Eq a => [a] -> [a] -> [a]
removeDupsH l acc =
    case l of
        []   -> acc
        x:xs -> if isIn x acc then removeDupsH xs acc else removeDupsH xs (acc++[x])

removeDups :: Eq a => [a] -> [a]
removeDups l = removeDupsH l []

insert :: Eq a => a -> Set a -> Set a
insert x s =
    case s of
        Cons xs -> Cons (removeDups (x:xs)) 

insertList :: Eq a => [a] -> Set a
insertList l = Cons (removeDups l)

member :: Eq a => a -> Set a -> Bool
member e s =
    case s of
        Cons l -> isIn e l

union :: Eq a => Set a -> Set a -> Set a
union s1 s2 =
    case (s1, s2) of
        (Cons l1, Cons l2) -> Cons (removeDups (l1++l2))

difference :: Eq a => Set a -> Set a -> Set a
difference s1 s2 =
    case (s1, s2) of
        (Cons l1, Cons l2) -> Cons (listDiff l1 l2)
                                where 
                                    listDiffH l1 l2 acc =
                                        case l1 of
                                            []   -> acc
                                            x:xs -> if isIn x l2 
                                                    then listDiffH xs l2 acc
                                                    else listDiffH xs l2 (acc++[x])
                                    listDiff l1 l2 = listDiffH l1 l2 [] 