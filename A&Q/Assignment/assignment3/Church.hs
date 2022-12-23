
module Church where

import Prelude
import Terms
import Subst
import Beta
import Booleans

zero :: Term
zero = Lambda "s" (Lambda "z" (Var "z"))

one :: Term
one = Lambda "s" (Lambda "z" (App (Var "s")(Var "z")))

two :: Term
two = Lambda "s" (Lambda "z" (App (Var "s") (App (Var "s")(Var "z"))))

three :: Term
three = Lambda "s" (Lambda "z" (App (Var "s") (App (Var "s") (App (Var "s")(Var "z")))))

four :: Term
four = Lambda "s" (Lambda "z"  (App (Var "s") (App (Var "s") (App (Var "s") (App (Var "s")(Var "z"))))))

five :: Term
five = Lambda "s" (Lambda "z"  (App (Var "s") (App (Var "s") (App (Var "s") (App (Var "s") (App (Var "s")(Var "z")))))))

numeralH :: Int -> Term -> Term
numeralH n acc =
    if n < 0
    then error "negative integer input in Church numeral calc"
    else if n == 0
    then acc
    else numeralH (n-1) (App (Var "s") acc)

numeral :: Int -> Term
numeral n = Lambda "s" (Lambda "z" (numeralH n (Var "z")))

add :: Term
add = Lambda "M"
      (
        Lambda "N"
        (
            Lambda "s"
            (
                Lambda "x"
                (
                    App (App(Var "N")
                            (Var "s"))
                        (App (App(Var "M")(Var "s"))(Var "x"))
                )
            )
        )
      )

addition :: Term -> Term -> Term
addition n m = App (App (add)(n))(m)

mult :: Term
mult =
    Lambda "M"
    (
        Lambda "N"
        (
            Lambda "s"
            (
                Lambda "z"
                (
                    App(App(Var "N")(App(Var "M")(Var "s")))(Var "z")
                )
            )
        )
    )

multiplication :: Term -> Term -> Term
multiplication m n =
    App(App(mult)(m))(n)

predH :: Term
predH =
    Lambda "n"
    (
        Lambda "s"
        (
            Lambda "z"
            (
                App(App(App(Var "n")
                           (Lambda "g"(Lambda "h" (App(Var "h")(App(Var "g")(Var "s"))))))
                        (Lambda "u" (Var "z")))
                    (Lambda "u" (Var "u"))
            )
        )
    )

predecessor :: Term -> Term
predecessor n = App predH n

subtr :: Term
subtr =
    Lambda "m"
    (
        Lambda "n"
        (
            App(App(Var "n")(predH))(Var "m")
        )
    )

subtraction :: Term -> Term -> Term
subtraction m n = App (App(subtr)(m))(n)

isZeroH :: Term
isZeroH =
    Lambda "n"
    (
        App (App(Var "n")
                (Lambda "x" (false)))
            (true)
    )

isZero :: Term -> Term
isZero n = App isZeroH n

leqH :: Term
leqH =
    Lambda "m"
    (
        Lambda "n"
        (
            isZero (subtraction (Var "m")(Var "n"))
        )
    )

leq :: Term -> Term -> Term
leq m n =
    App(App(leqH)(m))(n)

eqH :: Term
eqH =
    Lambda "m"
    (
        Lambda "n"
        (
            Booleans.and (leq (Var "m")(Var "n"))
                         (leq (Var "n")(Var "m"))
        )
    )

eq :: Term -> Term -> Term
eq m n =
    App(App(eqH)(m))(n)