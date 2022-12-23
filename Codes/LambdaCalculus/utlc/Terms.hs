{-# LANGUAGE GADTs #-}

module Terms where

import Prelude

data Term where
    Var    :: String -> Term
    Lambda :: String -> Term -> Term
    App    :: Term   -> Term -> Term 

term2String :: Term -> String
term2String t =
    case t of
        Var y       -> y
        Lambda y t1 -> "(λ" ++ y ++ ". " ++ term2String t1 ++ ")" 
        -- if λ is not properly printed out, you could use "\\" instead. Namely "(\\" ++ y ++ ". " ++ term2String t1 ++ ")" 
        App t1 t2   -> "[" ++ term2String t1 ++ " " ++ term2String t2 ++ "]"

instance Show Term where
    show t = term2String t