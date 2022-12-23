
module Booleans where

import Prelude
import Terms
import Subst
import Beta

true :: Term
true = Lambda "x" (Lambda "y" (Var "x"))

false :: Term
false = Lambda "x" (Lambda "y" (Var "y"))

notH :: Term
notH = Lambda "x" (Lambda "y" (Lambda "z" (App (App (Var "x")(Var "z"))(Var "y"))))

not :: Term -> Term
not t = App notH t

andH :: Term
andH = 
    Lambda "x"
    (
        Lambda "y"
        (
            App(App(Var "x")(Var "y"))(Var "x")
        )
    )  

and :: Term -> Term -> Term
and p q = App(App(andH)(p))(q)

orH :: Term
orH = 
    Lambda "x"
    (
        Lambda "y"
        (
            App(App(Var "x")(Var "x"))(Var "y")
        )
    )  

or :: Term -> Term -> Term
or p q = App(App(orH)(p))(q)


iteH :: Term
iteH = Lambda "a" (Lambda "b" (Lambda "c" (App (App (Var "a")(Var "b"))(Var "c"))))

ite :: Term -> Term -> Term -> Term
ite b t1 t2 = App (App(App(iteH)(b))(t1))(t2)