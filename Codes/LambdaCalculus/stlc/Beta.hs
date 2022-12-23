module Beta where

import Prelude
import Types
import Terms
import TypeCheck
import Subst

beta :: Term -> Term 
beta e =
    case e of
        App (Lambda y t1 e1) e2 -> subst e1 y e2
        App e1 e2               -> App (beta e1) (beta e2)
        Lambda y t1 e1          -> Lambda y t1 (beta e1)
        Fst (Pair e1 e2)        -> e1
        Snd (Pair e1 e2)        -> e2
        Plus e1 e2              -> case (e1, e2) of
                                    (ConstI n, ConstI m) -> ConstI (n+m)
                                    _                    -> Plus (beta e1) (beta e2)
        Mult e1 e2              -> case (e1, e2) of
                                    (ConstI n, ConstI m) -> ConstI (n*m)
                                    _                    -> Mult (beta e1) (beta e2)
        Minus e1 e2             -> case (e1, e2) of
                                    (ConstI n, ConstI m) -> ConstI (n-m)
                                    _                    -> Minus (beta e1) (beta e2)    
        IsEq e1 e2              -> case (e1, e2) of
                                    (ConstI n, ConstI m) -> ConstB (n==m)
                                    (ConstB n, ConstB m) -> ConstB (n==m)
                                    _                    -> IsEq (beta e1) (beta e2)
        Ite (ConstB True) e1 e2 -> e1
        Ite (ConstB False) e1 e2 -> e2
        Ite e1 e2 e3             -> Ite (beta e1) e2 e3
        Fix (Lambda y t1 e1)     -> subst e1 y (Fix (Lambda y t1 e1))
        Fix e1                   -> Fix (beta e1)
        _                        -> e 

refl_trans_beta :: Term -> Term
refl_trans_beta e = 
    let e' = beta e
    in if e' == e then e else refl_trans_beta e'

typed_beta :: Ctx -> Term -> Term
typed_beta m e =
    let t = typeCheck m e
    in case t of
        Yes ty -> beta e
        No s   -> error s

refl_trans_typed_beta :: Ctx -> Term -> Term
refl_trans_typed_beta m e =
    let t = typeCheck m e
    in case t of
        Yes yt -> refl_trans_beta e 
        No s   -> error s

