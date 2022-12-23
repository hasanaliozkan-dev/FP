
{-# LANGUAGE GADTs #-}

module Types where

import Prelude

data Type where
    TInt  :: Type
    TBool :: Type
    TUnit :: Type
    TVar  :: String -> Type
    Arrow :: Type   -> Type -> Type
    Prod  :: Type   -> Type -> Type

type2String :: Type -> String
type2String t =
    case t of
        TInt        -> "Z"
        TBool       -> "B"
        TUnit       -> "1"
        TVar s      -> s
        Arrow t1 t2 -> "(" ++ type2String t1 ++ " -> " ++ type2String t2 ++ ")"
        Prod t1 t2 -> "(" ++ type2String t1 ++ " * " ++ type2String t2 ++ ")"

instance Show Type where
    show t = type2String t

typeEq :: Type -> Type -> Bool
typeEq t1 t2 =
    case (t1, t2) of
        (TInt, TInt)               -> True
        (TBool, TBool)             -> True
        (TUnit, TUnit)             -> True
        (TVar s1, TVar s2)         -> s1 == s2
        (Arrow t1 t2, Arrow t3 t4) -> typeEq t1 t3 && typeEq t2 t4
        (Prod t1 t2, Prod t3 t4)   -> typeEq t1 t3 && typeEq t2 t4
        (_,_)                      -> False

instance Eq Type where
    t1 == t2 = typeEq t1 t2



