--Correctness of a compiler
--Source Language

data Expr = Val Int | Add Expr Expr 


--Semantic of the source language
eval :: Expr -> Int
eval e = 
    case e of 
        Val n -> n
        Add x y -> eval x + eval y

--Target Language
--Virtual Machine
type Stack = [Int]
type Code = [Op]
data Op = PUSH Int | ADD 

--create a stack and push something to into it 
--exec [PUSH 3, PUSH 4, ADD] [] = [7]


--Semantic of the target language
exec :: Code -> Stack -> Stack
exec c s = 
    case c of 
        [] -> s
        PUSH n : c -> exec c (n:s)
        ADD : c -> 
            case s of 
                x:y:s -> exec c (x+y:s)
                _ -> error "ADD: too few arguments on the stack" 
--Compiler
comp :: Expr -> Code
comp e = 
    case e of 
        Val n -> [PUSH n]
        Add x y -> comp x ++ comp y ++ [ADD]
--Compiler  
--Theorem: For all expressions e, exec (comp e) s = eval e : s

{-
                eval
        Expr ---------> Int
        |                |       
  comp  |       :)       | (:s)    exec (comp e) s = eval e : s
        |                |
        v                v
      Code ---------> Stack
             (`exec` s)
-}



{-
eval :: Expr -> Int
comp :: Expr -> Code
exec :: Code -> Stack -> Stack

exec (comp e) s = eval e : s
-}


{-
exec (comp e) s = eval e : s

Base Case :  e = Val n

exec (comp (Val n)) s
= exec (PUSH n) s
= n : s
= eval (Val n) : s

Inductive Case : e = Add x y
exec (comp (Add x y)) s
= exec (comp x ++ comp y ++ [ADD]) s
= exec (comp x ++ (comp y ++ exec [ADD])) s
= exec (comp y ++ exec [ADD]) (exec (comp x) s) 
= exec (comp y ++ exec [ADD]) (eval x : s) 
= exec exec [ADD] (exec (comp y) (eval x : s)) 
= exec [ADD] (eval y : eval x : s) 
= (eval x + eval y) : s
= eval (Add x y) : s
-}

compfast :: Expr -> Code
compfast e = compfasthelper e []

compfasthelper :: Expr -> Code -> Code
compfasthelper e c = 
    case e of 
        Val n -> PUSH n : c
        Add x y -> compfasthelper x (compfasthelper y (ADD : c))


{-
exec (compfasthelper e c) s = exec c (eval e : s)

Base Case : e = Val n

exec (compfasthelper (Val n) c) s
= exec (PUSH n : c) s
= exec c (n : s)
= exec c (eval (Val n) : s)

Inductive Case : e = Add x y
exec (compfasthelper (Add x y) c) s
= exec (compfasthelper x (compfasthelper y (ADD : c))) s
= exec (compfasthelper y (ADD : c)) (eval x : s)
= exec (ADD : c) (eval y : eval x : s)
= exec c (eval x + eval y : s)
= exec c (eval (Add x y) : s)
= exec c (eval e : s)
-}