module Stack (pop) where


type (Stack a) = [a]

empty :: Stack a
empty = []


push :: a -> Stack a -> Stack a
push x l = x:l

pop :: Stack a -> (a,Stack a)
pop l = 
    case l of 
        [] -> error "It is not possible!!!!"
        x:xs -> (x,xs)




