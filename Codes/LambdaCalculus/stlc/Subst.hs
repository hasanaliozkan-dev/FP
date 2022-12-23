
module Subst where

import Prelude
import Terms
import Data.IORef
import System.IO.Unsafe
import Types
import TypeCheck

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
fvH e acc =
    case e of
        Var y          -> y : acc 
        Lambda y t1 e1 -> filter (\a -> a /= y) (fvH e1 acc)
        App e1 e2      -> unique (fvH e1 acc ++ fvH e2 acc)
        Pair e1 e2     -> unique (fvH e1 acc ++ fvH e2 acc)
        Fst e1         -> unique (fvH e1 acc)
        Snd e1         -> unique (fvH e1 acc)
        Plus e1 e2     -> unique (fvH e1 acc ++ fvH e2 acc)
        Mult e1 e2     -> unique (fvH e1 acc ++ fvH e2 acc)
        Minus e1 e2    -> unique (fvH e1 acc ++ fvH e2 acc)
        IsEq e1 e2     -> unique (fvH e1 acc ++ fvH e2 acc)
        Ite e1 e2 e3   -> unique (fvH e1 acc ++ fvH e2 acc ++ fvH e3 acc)
        Fix e1         -> unique (fvH e1 acc)
        _              -> acc

fv :: Term -> [String]
fv t = fvH t []

{-
fv (Lambda "x" (Lambda "y" (App (App (Lambda "z" (Lambda "v" (App (Var "z")(App (Var "z")(Var "v"))))) (App (Var "x") (Var "y")))(App (Var "z")(Var "u")))))
-}

inTerm :: String -> Term -> Bool
inTerm x e =
    case e of
        Var y          -> x == y
        Lambda y t1 e1 -> if x == y then True else inTerm x e1
        App e1 e2      -> inTerm x e1 || inTerm x e2
        Pair e1 e2     -> inTerm x e1 || inTerm x e2
        Fst e1         -> inTerm x e1
        Snd e1         -> inTerm x e1
        Plus e1 e2     -> inTerm x e1 || inTerm x e2
        Mult e1 e2     -> inTerm x e1 || inTerm x e2
        Minus e1 e2    -> inTerm x e1 || inTerm x e2
        IsEq e1 e2     -> inTerm x e1 || inTerm x e2
        Ite e1 e2 e3   -> inTerm x e1 || inTerm x e2 || inTerm x e3
        Fix e1         -> inTerm x e1 
        _              -> False 

replace :: String -> String -> Term -> Term
replace x y e =
    case e of
        Var s          -> if s == x then Var y else e
        Lambda s t1 e1 -> if s == x then Lambda y t1 (replace x y e1) else Lambda s t1 (replace x y e1)
        App e1 e2      -> App (replace x y e1) (replace x y e2)
        Pair e1 e2     -> Pair (replace x y e1) (replace x y e2)
        Fst e1         -> Fst (replace x y e1)
        Snd e1         -> Snd (replace x y e1)
        Plus e1 e2     -> Plus (replace x y e1) (replace x y e2)
        Mult e1 e2     -> Mult (replace x y e1) (replace x y e2)
        Minus e1 e2    -> Minus (replace x y e1) (replace x y e2)
        IsEq e1 e2     -> IsEq (replace x y e1) (replace x y e2)
        Ite e1 e2 e3   -> Ite (replace x y e1) (replace x y e2) (replace x y e3)
        Fix e1         -> Fix (replace x y e1)
        _              -> e

alpha :: String -> Term -> Term
alpha x e =
    case e of
        Var y          -> Var y
        Lambda y t1 e1 -> if y /= x && inTerm x e1 == False 
                          then Lambda x t1 (replace y x e1)
                          else e
        App e1 e2   -> App (alpha x e1) (alpha x e2)
        Pair e1 e2  -> Pair (alpha x e1) (alpha x e2)
        Fst e1      -> Fst (alpha x e1)
        Snd e1      -> Snd (alpha x e1) 
        Plus e1 e2  -> Plus (alpha x e1) (alpha x e2)
        Mult e1 e2  -> Mult (alpha x e1) (alpha x e2)
        Minus e1 e2 -> Minus (alpha x e1) (alpha x e2)
        IsEq e1 e2  -> IsEq (alpha x e1) (alpha x e2)
        Ite e1 e2 e3 -> Ite (alpha x e1) (alpha x e2) (alpha x e3)
        Fix e1       -> Fix (alpha x e1)
        _            -> e

subst :: Term -> String -> Term -> Term
subst e x s =
    case e of
        Var y           -> if x == y then s else e
        Lambda y t1 e1  -> if y /= x && find y (fv s) == False
                           then Lambda y t1 (subst e1 x s)
                           else if y == x && find y (fv s) == False
                           then e
                           else let z  = freshVar y
                                    e' = alpha z e
                                in subst e' x s 
        App e1 e2   -> App (subst e1 x s) (subst e2 x s)
        Pair e1 e2  -> Pair (subst e1 x s) (subst e2 x s) 
        Fst e1      -> Fst (subst e1 x s) 
        Snd e1      -> Snd (subst e1 x s)
        Plus e1 e2  -> Plus (subst e1 x s) (subst e2 x s) 
        Mult e1 e2  -> Mult (subst e1 x s) (subst e2 x s) 
        Minus e1 e2 -> Minus (subst e1 x s) (subst e2 x s) 
        IsEq e1 e2  -> IsEq (subst e1 x s) (subst e2 x s) 
        Ite e1 e2 e3 -> Ite (subst e1 x s) (subst e2 x s) (subst e3 x s)
        Fix e1       -> Fix (subst e1 x s) 
        _            -> e

termEq :: Term -> Term -> Bool
termEq e1 e2 =
    case (e1, e2) of
        (Var s1, Var s2)                   -> s1 == s2
        (Lambda s1 t1 e1, Lambda s2 t2 e2) -> let fv  = freshVar "x"
                                                  e1' = subst e1 s1 (Var fv)
                                                  e2' = subst e2 s2 (Var fv)
                                              in termEq e1' e2' && typeEq t1 t2
        (App e1 e2, App e3 e4)             -> termEq e1 e3 && termEq e2 e4
        (Pair e1 e2, Pair e3 e4)           -> termEq e1 e3 && termEq e2 e4
        (Fst e1, Fst e2)                   -> termEq e1 e2
        (Snd e1, Snd e2)                   -> termEq e1 e2
        (Plus e1 e2, Plus e3 e4)           -> termEq e1 e3 && termEq e2 e4
        (Mult e1 e2, Mult e3 e4)           -> termEq e1 e3 && termEq e2 e4
        (Minus e1 e2, Minus e3 e4)         -> termEq e1 e3 && termEq e2 e4
        (IsEq e1 e2, IsEq e3 e4)           -> termEq e1 e3 && termEq e2 e4
        (Ite e1 e2 e3, Ite e4 e5 e6)       -> termEq e1 e4 && termEq e2 e5 && termEq e3 e6
        (Fix e1, Fix e2)                   -> termEq e1 e2
        (ConstI n, ConstI m)               -> n == m
        (ConstB n, ConstB m)               -> n == m
        (Tt, Tt)                           -> True
        (_, _)                             -> False

instance Eq Term where
    t1 == t2 = termEq t1 t2








