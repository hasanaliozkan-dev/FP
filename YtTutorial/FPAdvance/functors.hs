
import Prelude hiding (Functor , map, pure , (<*>),Maybe, Nothing, Just, fmap,Tree , Leaf , Node,Applicative)
inc :: [Int] -> [Int]
inc l = 
    case l of 
        [] -> l 
        (x:xs) -> x+1 : inc xs


sqr :: [Int] -> [Int]
sqr l = 
    case l of 
        [] -> []
        (x:xs) -> x^2 : sqr xs 

map :: (a-> b) -> [a] -> [b]
map f l =
    case l of 
        [] -> []
        x:xs -> f x : map f xs 

incOneLine :: [Int] -> [Int]
incOneLine = map (+1)

sqrOneLine :: [Int] -> [Int]
sqrOneLine = map (^2)

--Generalising further
class Functor f where
    fmap :: (a-> b) -> f a -> f b


--The list functor
instance Functor [] where
    fmap :: (a -> b) -> [a] -> [b]
    fmap = map

--Maybe Functor 
data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
    show :: Show a => Maybe a -> String
    show Nothing = "Nothing"
    show (Just x) = "Just " ++ show x

instance Functor Maybe where
    
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap g Nothing = Nothing
    fmap g (Just x) = Just (g x)

--Tree Functor

data Tree a = Leaf a
                | Node (Tree a) (Tree a)
 
instance Show a => Show (Tree a) where
    show :: Show a => Tree a -> String
    show (Leaf x) = "Leaf " ++ show x
    show (Node l r) = "Node (" ++ show l ++ ") (" ++ show r ++ ")"

t :: Tree Int
t = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)

instance Functor Tree where 
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap g (Leaf x) = Leaf (g x)
    fmap g (Node l r) = Node (Main.fmap g l) (Main.fmap g r) 

--Why use functors?
-- 1) We can use the same name for fmap , for function that are essentially the same.
-- 2) We can define generic functions that work for any functorial type 

incF ::  Functor f => f Int -> f Int
incF = Main.fmap (+1)

--Applicative functor
--------------------------------------------------------------
-- -fmap0 :: a -> f a                                       --                      
-- -fmap1 :: (a -> b) -> f a -> f b                         --
-- -fmap2 :: (a -> b -> c) -> f a -> f b -> f c             --
-- -fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d --
--------------------------------------------------------------

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where 
    pure :: a -> Maybe a
    pure a = Just a

    (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    (<*>) f a = 
        case f of 
            Nothing -> Nothing
            Just g -> Main.fmap g a

instance Applicative [] where
    pure :: a -> [a]
    pure a = [a]

    (<*>) :: [a -> b] -> [a] -> [b]
    (<*>) fs as = [f a | f <- fs, a <- as]


instance Applicative Tree where 
    pure :: a -> Tree a
    pure a = Leaf a

    (<*>) :: Tree (a -> b) -> Tree a -> Tree b
    (<*>) (Leaf g) t = Main.fmap g t
    (<*>) (Node l r) t = Node (l <*> t) (r <*> t)