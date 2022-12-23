{-# LANGUAGE GADTs #-}

module TypeCheck where

import Prelude
import Types
import Terms 

data Err a where
    Yes :: a      -> Err a
    No  :: String -> Err a
err2String :: Show a => Err a -> String
err2String e =
    case e of
        Yes e1 -> show e1
        No s   -> s

instance Show a => Show (Err a) where
    show e = err2String e

errEq :: Eq a => Err a -> Err a -> Bool
errEq e1 e2 =
    case (e1, e2) of
        (Yes i1, Yes i2) -> i1 == i2
        (No s1, No s2)   -> s1 == s2
        (_,_)            -> False

instance Eq a => Eq (Err a) where
    e1 == e2 = errEq e1 e2

type Ctx = [(String, Type)]

extend :: Ctx -> String -> Type -> Ctx
extend m s t = (s, t) : m 

myLookup :: Ctx -> String -> Err Type
myLookup m s =
    case m of
        []       -> No "no such term variable in the context"
        (x,t):xs -> if x == s then Yes t else myLookup xs s

typeCheck :: Ctx -> Term -> Err Type
typeCheck m e =
    case e of
        Var    s       -> myLookup m s
        Lambda y t1 e1 -> let m'  = extend m y t1
                              te1 = typeCheck m' e1
                          in case te1 of
                                Yes yte1 -> Yes (Arrow t1 yte1)
                                _        -> No "typing error in lambda"
        App e1 e2      -> let te1 = typeCheck m e1
                              te2 = typeCheck m e2
                          in case (te1, te2) of
                                (Yes (Arrow s t), Yes yte2) | s == yte2 -> Yes t
                                _                                       -> No "typing error in app"
        ConstI n       -> Yes TInt
        ConstB b       -> Yes TBool
        Tt             -> Yes TUnit
        Pair e1 e2     -> let te1 = typeCheck m e1
                              te2 = typeCheck m e2
                          in case (te1, te2) of
                                (Yes yte1, Yes yte2) -> Yes (Prod yte1 yte2)
                                _                    -> No "typing error in pair"
        Fst e1         -> let te1 = typeCheck m e1 
                          in case te1 of
                                Yes (Prod f s) -> Yes f
                                _              -> No "typing error in fst"
        Snd e1         -> let te1 = typeCheck m e1 
                          in case te1 of
                                Yes (Prod f s) -> Yes s
                                _              -> No "typing error in snd"
        Plus e1 e2     -> let te1 = typeCheck m e1
                              te2 = typeCheck m e2
                          in case (te1, te2) of
                                (Yes TInt, Yes TInt) -> Yes TInt
                                (_,_)                -> No "typing error in plus"
        Mult e1 e2     -> let te1 = typeCheck m e1
                              te2 = typeCheck m e2
                          in case (te1, te2) of
                                (Yes TInt, Yes TInt) -> Yes TInt
                                (_,_)                -> No "typing error in mult"
        Minus e1 e2    -> let te1 = typeCheck m e1
                              te2 = typeCheck m e2
                          in case (te1, te2) of
                                (Yes TInt, Yes TInt) -> Yes TInt
                                (_,_)                -> No "typing error in minus"
        IsEq e1 e2     -> let te1 = typeCheck m e1
                              te2 = typeCheck m e2
                          in if te1 == te2 then Yes TBool else No "typing error in iseq"
        Ite e1 e2 e3   -> let te1 = typeCheck m e1
                              te2 = typeCheck m e2
                              te3 = typeCheck m e3
                          in if te1 == Yes TBool && te2 == te3 then te2 else No "typing error in ite"
        Fix e1         -> let te1 = typeCheck m e1
                          in case te1 of 
                                Yes (Arrow s t) | s == t -> Yes t
                                _                        -> No "typing error in fix"
{-
let t = Lambda "z" (Prod (Arrow (TVar "A")(TVar "B"))(Arrow (TVar "A")(TVar "C")))(Lambda "x" (TVar "A")(Pair (App(Fst (Var "z"))(Var "x"))(App(Snd (Var "z"))(Var "x"))))
-}