-- 1) Induction on Natural Numbers
 

{-
P(Zero) forall [P(n) -> P(Succ n)]
---------------------------------
          forall P(n)
-}
data Nat = Zero | Succ Nat

printNat :: Nat -> String
printNat Zero = "Zero"
printNat (Succ n) = "Succ " ++ "(  " ++ printNat n ++ ")"

instance Show Nat where
  show = printNat


natToInt :: Nat -> Int
natToInt n =
  case n of
    Zero -> 0
    Succ n -> 1 + natToInt n

intToNat :: Int -> Nat
intToNat n =
  case n of
    0 -> Zero
    n -> Succ (intToNat (n - 1))

addNat :: Nat -> Nat -> Nat
addNat n1 n2 =
  case n1 of
    Zero -> n2
    Succ n -> Succ (addNat n n2)

substractNat :: Nat -> Nat -> Nat 
substractNat n1 n2 = 
        case n1 of 
            Zero -> n2
            Succ n -> 
                case n2 of 
                    Zero -> n
                    Succ n2 -> substractNat n n2

-- 2) Induction on Lists
{-
P([]) forall x. forall xs.[P(xs) -> P(x:xs)]
------------------------------------------------
            forall xs.P(xs)
-}


data MyList a = Nil | Cons a (MyList a)

printMyList :: Show a => MyList a -> String
printMyList n = 
        case n of 
            Nil -> "Nil"
            Cons x xs -> "Cons " ++ show x ++ " (" ++ printMyList xs ++ ")"

instance Show a => Show (MyList a) where
  show = printMyList

myListToList :: MyList a -> [a]
myListToList n =
  case n of
    Nil -> []
    Cons x xs -> x : myListToList xs


listToMyList :: [a] -> MyList a
listToMyList n =
  case n of
    [] -> Nil
    x : xs -> Cons x (listToMyList xs)

mergeTwoList :: MyList a -> MyList a -> MyList a
mergeTwoList n1 n2 =
  case n1 of
    Nil -> n2
    Cons x xs -> Cons x (mergeTwoList xs n2)
