module Fibonacci where

import Terms
import Subst
import Beta
import Booleans
import Church
import YCombinator
import Factorial

fib :: Term
fib =
    Lambda "f"
    (
        Lambda "x"
        (
            ite
            (isZero (Var "x"))
            (zero)
            (ite
                (isZero (predecessor (Var "x")))
                (one)
                (addition (App(Var "f")(predecessor (Var "x")))
                          (App(Var "f")(subtraction (Var "x")(two))))
            )
        )
    )

fibonacci :: Term -> Term
fibonacci n = App (App(yCombinator)(fib))(n)

fibTL :: Term
fibTL =
    Lambda "f"
    (
        Lambda "n"
        (
            Lambda "i"
            (
                Lambda "a"
                (
                    Lambda "b"
                    (
                        ite
                        (eq (Var "i")(Var "n"))
                        (Var "a")
                        (App(App(App(App(Var "f")
                                        (Var "n"))
                                    (addition (Var "i")(one)))
                                (Var "b"))
                            (addition (Var "a")(Var "b")))
                    )
                )
            )
        )
    )

fibonacciTL :: Term -> Term
fibonacciTL n = App(App(App(App(App(yCombinator)(fibTL))(n))(zero))(zero))(one)




