
module Pairs where


import Prelude
import Terms
import Subst
import Beta
import YCombinator
import Booleans
import Church
import Factorial
import Fibonacci

--pair := λx.λy.λƒ.ƒ x y


pair :: Term
pair = Lambda "x" 
        (
            Lambda "y" 
                (
                    Lambda "f" 
                        (
                            App (App (Var "f") (Var "x")) (Var "y")
                        )
                )
        )

--first :=λp.p true
first :: Term -> Term
first t = App t Booleans.true

--second :=λp.p false
second :: Term -> Term
second t = App t Booleans.false