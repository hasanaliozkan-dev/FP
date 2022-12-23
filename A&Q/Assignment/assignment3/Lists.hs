
module Lists where

import Prelude
import Terms
import Subst
import Beta
import YCombinator
import Booleans
import Church
import Factorial
import Fibonacci
import Pairs


--cons := λx.λl.pair false (pair x l)
cons :: Term -> Term -> Term
cons x l = App (App pair Booleans.false) (App (App pair x) l)


--nil := λl.l
nil :: Term
nil = Lambda "l" (Var "l")

--hd := λl.first (second l)
hd :: Term -> Term
hd t = first (second t)
--hd = Lambda "l" (App first (App second (Var "l")))


--tl := λl.second (second l).
tl :: Term -> Term
tl t = second (second t)
--tl = Lambda "l" (App second (App second (Var "l")))


--isNull := first
isNull :: Term -> Term
isNull t = first t 

--length := λƒ.λl (isNull l) (zero) (addition (one) (ƒ (tl l)))
lengthTerm :: Term 
lengthTerm  = Lambda "f"
            (
                Lambda "l"
                (
                    ite
                    (isNull (Var "l"))
                    (zero)
                    (addition (one) (App (Var "f") (tl (Var "l"))))
                )
            ) 

length :: Term -> Term
length t = App (App (yCombinator) (lengthTerm)) (t)


--append:= λƒ.λl1.λl2ite (isNull l1) (l2) (cons (hd l1) (ƒ (tl l1) l2))
appendTerm :: Term
appendTerm = Lambda "f"
            (
                Lambda "l1"
                (
                    Lambda "l2"
                    (
                        ite
                        (isNull (Var "l1"))
                        (Var "l2")
                        (cons (hd (Var "l1")) (App (App (Var "f") (tl (Var "l1"))) (Var "l2")))
                    )
                )
            )

append :: Term -> Term -> Term
append l1 l2 = App (App (App yCombinator appendTerm) l1) l2


--reverse := λƒ.λl1.λl2.ite (isNull l1) (l2) (ƒ (tl l1) (cons (hd l1) (l2))).
reverseTerm ::Term
reverseTerm = Lambda "f"
            (
                Lambda "l1"
                (
                    Lambda "l2"
                    (
                        ite
                        (isNull (Var "l1"))
                        (Var "l2")
                        (App (App (Var "f") (tl (Var "l1"))) (cons (hd (Var "l1")) (Var "l2")))
                    )
                )
            )

reverse :: Term -> Term
reverse l = App (App (App yCombinator reverseTerm) l) nil
