
module Subst where

import Prelude
import Terms
import Data.IORef
import System.IO.Unsafe


counterRef :: IORef Int
counterRef = unsafePerformIO (newIORef 0)

incrementCount :: IO Int
incrementCount = atomicModifyIORef counterRef (\c -> (c+1,c+1))

resetCount :: IO Int
resetCount = atomicModifyIORef counterRef (\c -> (0,0))

readCount :: IO Int
readCount = readIORef counterRef

freshVar :: String -> String
freshVar y = let x = unsafePerformIO incrementCount in  y ++ show x

find :: Eq a => a -> [a] -> Bool
find y l =
    case l of
        []   -> False
        x:xs -> if x == y then True else find y xs

uniqueH :: Eq a => [a] -> [a] -> [a]
uniqueH l acc =
    case l of
        []   -> acc
        x:xs -> if find x acc then uniqueH xs acc else uniqueH xs (acc ++ [x])

unique :: Eq a => [a] -> [a]
unique l = uniqueH l []

fvH :: Term -> [String] -> [String]
fvH t acc =
    case t of
        Var y       -> y : acc 
        Lambda y t1 -> filter (\a -> a /= y) (fvH t1 acc)
        App t1 t2   -> unique (fvH t1 acc ++ fvH t2 acc)

fv :: Term -> [String]
fv t = fvH t []

{-
fv (Lambda "x" (Lambda "y" (App (App (Lambda "z" (Lambda "v" (App (Var "z")(App (Var "z")(Var "v"))))) (App (Var "x") (Var "y")))(App (Var "z")(Var "u")))))
-}

inTerm :: String -> Term -> Bool
inTerm x t =
    case t of
        Var y       -> x == y
        Lambda y t1 -> if x == y then True else inTerm x t1
        App t1 t2   -> inTerm x t1 || inTerm x t2

replace :: String -> String -> Term -> Term
replace x y t =
    case t of
        Var s       -> if s == x then Var y else t
        Lambda s t1 -> if s == x then Lambda y (replace x y t1) else Lambda s (replace x y t1)
        App t1 t2   -> App (replace x y t1) (replace x y t2)

alpha :: String -> Term -> Term
alpha x t =
    case t of
        Var y       -> Var y
        Lambda y t1 -> if y /= x && inTerm x t1 == False 
                       then Lambda x (replace y x t1)
                       else t
        App t1 t2   -> App (alpha x t1) (alpha x t2)

subst :: Term -> String -> Term -> Term
subst t x s =
    case t of
        Var y       -> if x == y then s else t
        Lambda y t1 -> if y /= x && find y (fv s) == False
                       then Lambda y (subst t1 x s)
                       else if y == x && find y (fv s) == False
                       then t
                       else let z  = freshVar y
                                t' = alpha z t
                            in subst t' x s 
        App t1 t2   -> App (subst t1 x s) (subst t2 x s) 

termEq :: Term -> Term -> Bool
termEq t1 t2 =
    case (t1, t2) of
        (Var s1, Var s2)             -> s1 == s2
        (Lambda s1 e1, Lambda s2 e2) -> let fv  = freshVar "x"
                                            e1' = subst e1 s1 (Var fv)
                                            e2' = subst e2 s2 (Var fv)
                                        in termEq e1' e2'
        (App e1 e2, App e3 e4)        -> termEq e1 e3 && termEq e2 e4
        (_, _)                        -> False

instance Eq Term where
    t1 == t2 = termEq t1 t2








