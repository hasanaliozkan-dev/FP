{-# LANGUAGE GADTs #-}

module Terms where

import Prelude
import Types

data Term where
    Var    :: String -> Term
    Lambda :: String -> Type -> Term -> Term
    App    :: Term   -> Term -> Term
    ConstI :: Int    -> Term
    ConstB :: Bool   -> Term
    Tt     :: Term
    Pair   :: Term   -> Term -> Term
    Fst    :: Term   -> Term
    Snd    :: Term   -> Term
    Plus   :: Term   -> Term -> Term
    Mult   :: Term   -> Term -> Term
    Minus  :: Term   -> Term -> Term
    IsEq   :: Term   -> Term -> Term
    Ite    :: Term   -> Term -> Term -> Term
    Fix    :: Term   -> Term

whnf :: Term -> Bool
whnf e =
    case e of
        Var s                   -> True
        ConstI n                -> True
        ConstB b                -> True
        Tt                      -> True
        Lambda y t1 e1          -> True
        App (Lambda y t1 e1) e2 -> False
        App e1 e2               -> whnf e1
        _                       -> False


term2String :: Term -> String
term2String e =
    case e of
        Var y          -> y
        Lambda y t1 e1 -> "(λ" ++ y ++ ": " ++ type2String t1 ++ ". " ++ term2String e1 ++ ")" 
        App e1 e2      -> "[" ++ term2String e1 ++ " " ++ term2String e2 ++ "]"
        ConstI n       -> show n
        ConstB b       -> show b
        Tt             -> "tt"
        Pair e1 e2     -> "(" ++ term2String e1 ++ ", " ++ term2String e2 ++ ")"
        Fst e1         -> "(pi1 " ++ term2String e1 ++ ")"
        Snd e1         -> "(pi2 " ++ term2String e1 ++ ")" 
        Plus e1 e2     -> "(" ++ term2String e1 ++ " + " ++ term2String e2 ++ ")"
        Mult e1 e2     -> "(" ++ term2String e1 ++ " x " ++ term2String e2 ++ ")"        
        Minus e1 e2    -> "(" ++ term2String e1 ++ " - " ++ term2String e2 ++ ")"
        IsEq e1 e2     -> "(" ++ term2String e1 ++ " == " ++ term2String e2 ++ ")"
        Ite e1 e2 e3   -> "if " ++ term2String e1 ++ "then { " ++ term2String e2 ++ "} else {" ++ term2String e3 ++ "}"
        Fix e1         -> "fix" ++ term2String e1

instance Show Term where
    show e = term2String e

