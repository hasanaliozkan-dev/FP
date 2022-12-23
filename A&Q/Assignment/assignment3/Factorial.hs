module Factorial where

import Terms
import Subst
import Beta
import Booleans
import Church
import YCombinator

fact :: Term
fact =
    Lambda "f"
    (
        Lambda "x"
        (
            ite
            (isZero (Var "x"))
            (one)
            (multiplication (Var "x")(App(Var "f")(predecessor (Var "x"))))
        )
    )

factorial :: Term -> Term
factorial n = App(App(yCombinator)(fact))(n)
