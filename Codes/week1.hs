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


myEq :: Nat -> Nat -> Bool
myEq n m = 
    case n of 
        O   ->
            case m of
                O   -> True
                S l -> False
        S k ->
            case m of 
                O   -> False
                S l -> myEq k l

instance Eq Nat where
    n == m = myEq n m


myLeq :: Nat-> Nat -> Bool
myLeq n m = 
    case n of   
        O   -> True
        S k ->
            case m of
                O   -> False
                S l -> myLeq k l

instance Ord Nat where
    n <= m = myLeq n m
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

myMult :: Nat -> Nat -> Nat
myMult n m =
    case n of
        O  -> 0
        S k -> myAdd m (myMult k m)

mySubstr :: Nat -> Nat -> Nat
mySubstr n m =
    case m of
        O -> n
        S l ->
            case n of
                O -> 0
                S k -> mySubstr k l
mySignum :: Nat -> Nat
mySignum n = 
    case n of 
        O -> 0
        S k -> 1
myAbs :: Nat -> Nat
myAbs n = n

myFromInteger :: Integer -> Nat
myFromInteger i = 
    if i <= 0 then O else S (myFromInteger (i-1)) 
dropEmpty :: String-> String
dropEmpty s = 
    case s of
        [] -> []
        x:xs -> if x == ' ' then dropEmpty xs else x: dropEmpty xs
myString2NatH :: String -> Nat
myString2NatH s = 
    case s of
        [] -> error "no Nat instance given"
        [x] -> if x == 'O' then O else error  "not a Nat shape instance given"
        x:xs -> if x == 'S' then S (myString2NatH xs) else error  "not a Nat shape instance given"
myString2Nat :: String -> Nat
myString2Nat s =
    let str = dropEmpty s in myString2NatH str  

instance Read Nat where
    readsPrec _ l = 
        [(myString2Nat l, "")]
instance Num Nat where
    n + m = myAdd n m
    n - m = mySubstr n m
    n * m = myMult n m
    signum n = mySignum n
    abs n = myAbs n
    fromInteger n = myFromInteger n 

{- type classes -}

{- declaring the types Pair a b, List a and Nat as Show class instances -}

instance Show a => Show (List a) where
    show l = printList l

-- Pair 20 42.2 --> (20,42.2)

instance (Show a, Show b) => Show (Pair a b) where
    show l = printPair l

-- (myConcat (Cons 20 Nil) (Cons 10 (Cons 50 Nil))) --> [20,10,50]

instance Show Nat where
    show n = show (printNat n)

-- myAdd (S O) (S (S O)) --> 3

myImplies :: Bool -> Bool -> Bool 
myImplies p q = 
    case p of 
        True -> 
            case q of
                True -> True
                False -> False
        _   -> True 


-- Create a function that takes the first n elements of a list and returns it
myTake :: Int -> List a -> List a
myTake n l =
    case n of
        0 -> Nil
        _ -> 
            case l of
                Nil -> Nil
                Cons x xs -> Cons x (myTake (n-1) xs)

-- Create a function that drops the first n elements of a list and returns it
myDrop :: Int -> List a -> List a
myDrop n l =
    case n of
        0 -> l
        _ -> 
            case l of
                Nil -> Nil
                Cons x xs -> myDrop (n-1) xs


-- Create Rational type
data Rational where
    Rat :: Int -> Int -> Rational

--Implement operation for adding two rational numbers
myAddRat :: Rational -> Rational -> Rational
myAddRat (Rat a b) (Rat c d) = Rat (a*d + b*c) (b*d)

-- Implement operation for multiplying two rational numbers
myMultRat :: Rational -> Rational -> Rational
myMultRat (Rat a b) (Rat c d) = Rat (a*c) (b*d)

