
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

add :: Term
add = Lambda "M"
      (
        Lambda "N"
        (
            Lambda "f"
            (
                Lambda "x"
                (
                    App (App(Var "N")
                            (Var "f"))
                        (App (App(Var "M")(Var "f"))(Var "x"))
                )
            )
        )
      )

addition :: Term -> Term -> Term
addition n m = App (App (add)(n))(m)