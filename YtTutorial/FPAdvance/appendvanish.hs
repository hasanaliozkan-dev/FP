
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


