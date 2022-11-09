

module Beta where

import Prelude
import Terms
import Subst

beta :: Term -> Term
beta t =
    case t of
        App (Lambda y t1) t2 -> subst t1 y t2
        App t1 t2            -> App (beta t1) (beta t2)
        Lambda y t1          -> Lambda y (beta t1)
        _                    -> t

refl_tras_beta :: Term -> Term
refl_tras_beta t =
    let t' = beta t
    in if t == t' then t else refl_tras_beta t'


cbn :: Term -> Term
cbn t =
    case t of
        App (Lambda y t1) t2 -> subst t1 y t2
        App t1 t2            -> App (cbn t1) t2
        _                    -> t

cbv :: Term -> Term
cbv t =
    case t of
        App (Lambda y t1) t2 -> if t2 == cbv t2
                                then subst t1 y t2
                                else t
        App t1 t2            -> if t1 == cbv t1
                                then App t1 (cbn t2)
                                else App (cbn t1) t2
        _                    -> t

normal :: Term -> Term
normal t =
    case t of
        App (Lambda y t1) t2 -> subst t1 y t2
        App t1 t2            -> if t1 == normal t1
                                then App t1 (normal t2)
                                else App (normal t1) t2
        Lambda y t1          -> Lambda y (normal t1)
        _                    -> t

applicative :: Term -> Term
applicative t =
    case t of
        App (Lambda y t1) t2 -> if t2 == applicative t2
                                then subst t1 y t2
                                else App (Lambda y t1) (applicative t2)
        App t1 t2            -> if t1 == applicative t1
                                then App t1 (applicative t2)
                                else App (applicative t1) t2
        Lambda y t1          -> Lambda y (applicative t1)
        _                    -> t