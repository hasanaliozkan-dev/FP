
data Op = Add | Sub | Mul | Div

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

data Expr = Val Int | App Op Expr Expr  
{-
printExpr :: Expr -> String
printExpr e =
  case e of
    Val i -> show i
    App o e1 e2 -> printExpr e1 ++ "+" ++ printExpr e2


printLExpr :: [Expr] -> String
printLExpr l =
  case l of
    []  -> ""
    x:xs -> printExpr x ++ " " ++ printLExpr xs

instance Show Expr where
  show :: Expr -> String
  show e = printExpr e
-}

instance Show Expr where
  show (Val n)      = show n
  show (App o l r)  = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e       = "(" ++ show e ++ ")"



{-

choices :: [a] -> [[a]]
choices [] = [[]]
choices (x:xs) = [x:ys | ys <- choices xs] ++ choices xs
-}

eval :: Expr -> [Int]
eval (Val n) = [n|n>0]
eval (App o l r) = [apply o x y | x <- eval l
                                , y <- eval r
                                , valid o x y] 

choices :: [a] -> [[a]]
choices xs = [ p | s <- subs xs, p <- perms s]

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where
    yss = subs xs

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)


values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r


solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

exprs :: [Int]-> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns
              , l       <- exprs ls
              , r       <- exprs rs
              , e       <- combine l r]

combine :: Expr -> Expr ->[Expr]
combine l r = 
  [App o l r | o <- [Add,Sub,Mul,Div]]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns'   <- choices ns
                    ,e      <- exprs ns'
                    ,eval e == [n]]