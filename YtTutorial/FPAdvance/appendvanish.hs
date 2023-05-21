
--Fast Reverse

{-
How many steps does `xs ++ ys` take?
    [1,2] ++ [3]
    = 1 : ([2] ++ [3])
    = 1 : 2 : ([] ++ [3])  -> base case of the `++`
    = 1 : 2 : [3]

    `xs ++ ys` takes `length xs` + 1  steps
-}
{-
How many steps does `reverse xs` take?
    reverse [1,2]
    = reverse [2] ++ [1]                                         ¯|
    = (reverse [] ++ [2]) ++ [1]  -> base case of the `reverse`   | -> 3 steps
    = ([] ++ [2]) ++ [1]  -> base case of the `++`               _|
      |__________|
            |
          1 step 
      |________________|
                |
              2 steps
    
    `reverse xs` 1 +2 + 3 + ... + (n+1) steps, where n = length xs  -> (n^2 + 3*n +2) /2
-}


{-
fastreversehelper (x:xs) ys = (reverse xs) ++ ys

Base case:
fastreversehelper [] ys = ys

Inductive case:

fastreversehelper (x:xs) ys 
= reverse (x:xs) ++ ys 
= (reverse xs ++ [x]) ++ ys
= reverse xs ++ ([x] ++ ys)
= reverse xs ++ (x:ys)
= fastreversehelper xs (x:ys)

fastreversehelper (x:xs) ys = fastreversehelper xs (x:ys)

-}
fastreversehelper :: [a] -> [a] -> [a]
fastreversehelper l1 l2 = 
            case l1 of 
                [] -> l2
                (x:xs) -> fastreversehelper xs (x:l2)

fastreverse :: [a] -> [a]
fastreverse l = fastreversehelper l []

{-
|------------------------------------------------|
|rev:: [a] -> [a]             |                  | 
|rev [] = []                  | Slow :(          |
|rev (x:xs) = rev xs ++ [x]   | Quadratic        |  
|------------------------------------------------|
|------------------------------------------------|
|rev:: [a] -> [a]             |                  |         
|rev xs = revhelper xs []     | Fast :)          |
|                             |                  |
|revhelper:: [a] -> [a] -> [a]| Linear           |
|revhelper [] ys = ys         |                  |
|revhelper (x:xs) ys =        |                  |
|         revhelper xs (x:ys) |                  |
|------------------------------------------------|
-}


--Fast Flatten 

data Tree = Leaf Int | Node Tree Tree

flattenslow :: Tree -> [Int]
flattenslow t = 
        case t of 
            Leaf x -> [x]
            Node t1 t2 -> flattenslow t1 ++ flattenslow t2

{-

flattenhelper t ys = flatten t ++ ys

Base case:
flattenhelper (Leaf x) ys
= flatten (Leaf x) ++ ys
= [x] ++ ys
= x:ys

Inductive case:
flattenhelper (Node l r) ys
= flatten (Node l r) ++ ys
= (flatten l ++ flatten r) ++ ys
= flatten l ++ (flatten r ++ ys)
= flatten l ++ (flattenhelper r ys)
= flattenhelper l (flattenhelper r ys)

flattenhelper (Node l r) ys = flattenhelper l (flattenhelper r ys)
-}

flattenhelper :: Tree -> [Int] -> [Int]
flattenhelper t ys = 
        case t of 
            Leaf x -> x:ys
            Node l r -> flattenhelper l (flattenhelper r ys)


flatten :: Tree -> [Int]
flatten t = flattenhelper t []
