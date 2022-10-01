{-# LANGUAGE GADTs #-}
--Generalised Algebric Datatypes
import Prelude

{- List a type -}

data List a where
    Nil  :: List a
    Cons :: a -> List a -> List a

printListH :: Show a => List a -> String
printListH l =
    case l of
        Nil        -> ""
        Cons x Nil -> show x
        Cons x xs  -> show x ++ "," ++ printListH xs

printList :: Show a => List a -> String
printList l = "[" ++ printListH l ++ "]"

-- printList (Cons 10 (Cons 30 Nil)) --> [10,30]

myConcat :: List a -> List a -> List a
myConcat l1 l2 =
    case l1 of
        Nil       -> l2
        Cons x xs -> Cons x (myConcat xs l2) 

-- printList (myConcat (Cons 20 Nil) (Cons 10 (Cons 50 Nil))) --> [20,10,50]


{- Pair a b type -}

data Pair a b where
    Pair :: a -> b -> Pair a b

printPair :: (Show a, Show b) => Pair a b -> String
printPair p =
    case p of
        Pair x y -> "(" ++ show x ++ "," ++ show y ++ ")"

-- printPair (Pair 10 False) --> (10,False)

first :: Pair a b -> a
first p =
    case p of
        Pair x y -> x

-- first (Pair True 45) --> True

second :: Pair a b -> b
second p =
    case p of
        Pair x y -> y

-- second (Pair True 45) --> 45

{- arrow (function) types -}

add :: Int -> Int -> Int
add x y = x + y

addp :: (Int, Int) -> Int
addp x = 
    case x of
        (a, b) -> a + b

{- Currifying -}

myCurry :: ((a, b) -> c) -> (a -> b -> c)
myCurry f x y = f (x, y)

-- myCurry addp 10 20 --> 30

{- Decurrifying -}

myUnCurry :: (a -> b -> c) -> ((a, b) -> c)
myUnCurry f x =
    case x of
        (a, b) -> f a b

-- myUnCurry add (10,20) --> 30

{- partial application of the add function gives us the successor function as follows -}

mySucc :: Int -> Int
mySucc = add 1

{- anonymous functions (Lambda abstractions) -}

add' :: Int -> Int -> Int
add' = \x -> \y -> x + y

{- Nat type -}

data Nat where
    O :: Nat
    S :: Nat -> Nat

printNat :: Nat -> Int
printNat n =
    case n of
        O   -> 0
        S k -> 1 + (printNat k)

-- printNat (S (S (S (S O)))) --> 4

myAdd :: Nat -> Nat -> Nat
myAdd n m =
    case n of
        O   -> m
        S k -> S (myAdd k m)

-- printNat (myAdd (S O) (S (S O))) --> 3

{- TODO: (myMult :: Nat -> Nat -> Nat) implement a function that takes "a pair of" Nat instances and multiplies them together -}

{- type classes -}

{- declaring the types Pair a b, List a and Nat as Show class instances -}

instance (Show a => Show (List a)) where
    show l = printList l

-- Pair 20 42.2 --> (20,42.2)

instance ((Show a, Show b) => Show (Pair a b)) where
    show l = printPair l

-- (myConcat (Cons 20 Nil) (Cons 10 (Cons 50 Nil))) --> [20,10,50]

instance Show Nat where
    show n = show (printNat n)

-- myAdd (S O) (S (S O)) --> 3



