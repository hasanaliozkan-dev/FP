import Prelude hiding (Monad, (>>=), (>>), fail, return,mapM)
import qualified Control.Monad as Ghc.Base
import Data.Char ( digitToInt, isDigit )
import GHC.Cmm.Graph (mkJumpExtra)
data Expr = Val Int | Div Expr Expr

eval :: Expr -> Int
eval e = 
    case e of 
        Val n -> n
        Div x y -> eval x `div` eval y 

safeeval :: Expr -> Maybe Int
safeeval e =
    case e of 
        Val n -> Just n
        Div x y -> 
            case safeeval x of 
                Nothing -> Nothing
                Just n -> 
                    case safeeval y of 
                        Nothing -> Nothing
                        Just m -> safediv n m

applicativeeval :: Expr -> Maybe Int
applicativeeval e = 
    case e of 
        Val n -> Just n
        Div x y -> pure div <*> applicativeeval x <*> applicativeeval y


safediv :: Int -> Int -> Maybe Int
safediv n m = 
    case m of 
        0 -> Nothing 
        m -> Just (n `div` m)

e :: Expr
e = Div (Val 6) (Val 3)
-- >>=
bindingoperator :: Maybe a -> (a -> Maybe b) -> Maybe b
bindingoperator = flip bindMaybe

bindMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe f Nothing = Nothing
bindMaybe f (Just x) = f x

neweval :: Expr -> Maybe Int
neweval e = 
    case e of 
        Val n -> Just n
        Div x y -> 
            neweval x `bindingoperator` \n ->
            neweval y `bindingoperator` \m ->
            safediv n m
-----------------
-- do notation --
-- x1 <- m1    --
-- x2 <- m2    --
-- ...         --
-----------------

doeval :: Expr -> Maybe Int
doeval e = 
    case e of 
        Val n -> Just n
        Div x y -> do 
            n <- doeval x
            m <- doeval y
            safediv n m

class Applicative m => Monad m where 
    bindingoperatorm :: m a -> (a -> m b) -> m b
    return :: a -> m a
    return = pure

instance Monad Maybe where
    --bindingoperatorm :: Maybe a -> (a -> Maybe b) -> Maybe b
    bindingoperatorm Nothing f = Nothing
    bindingoperatorm (Just x) f = f x  
    return = Just   

instance Monad [] where 
    --bindingoperatorm :: [a] -> (a -> [b]) -> [b]
    bindingoperatorm xs f = concat (map f xs)
    return x = [x]
    

-- pair xs ys = [(x,y)| x <- xs, y <- ys]
monadicPair :: [a] -> [b] -> [(a,b)]
monadicPair xs ys = do 
    x <- xs
    y <- ys
    return (x,y)

--State transformer
type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

--instance Monad ST where 
    --bindingoperatorm :: ST a -> (a -> ST b) -> ST b
--    bindingoperatorm st f = S(\s -> let (x,s') = app st s in app (f x) s')

    --return :: a-> ST a
--    return x = S(\s -> (x,s))

--relabelling trees
data Tree a = Leaf a | Node (Tree a) (Tree a)

--print tree 
instance Show a => Show (Tree a) where 
    show (Leaf x) = show x
    show (Node l r) = "(" ++ show l ++ "," ++ show r ++ ")"
    
t :: Tree Char
t = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf x) n = (Leaf n, n+1)
rlabel (Node l r) n = (Node l' r', n'')
    where 
        (l', n') = rlabel l n
        (r', n'') = rlabel r n'
fresh :: ST Int 
fresh = S(\n -> (n, n+1))

{-

mlabel:: Tree a -> ST (Tree Int)
mlabel (Leaf x) = do 
    n <- fresh
    return (Leaf n)
mlabel (Node l r) = do
    l' <- mlabel l
    r' <- mlabel r
    return (Node l' r')
    

label :: Tree a -> Tree Int 
label t = fst (app (mlabel t) 0)
-}


mapM :: (Monad m, Ghc.Base.Monad m) => (a -> m b) -> [a] -> m [b]
mapM f [] = return []
mapM f (x:xs) = do 
    y <- f x
    ys <- mapM f xs
    return (y:ys)

conv :: Char -> Maybe Int
conv c | isDigit c = Just (digitToInt c)
       | otherwise = Nothing

--concat :: [[a]] -> [a]
--concat xss = [x | xs <- xss, x <- xs]

--flattened we can use this in flatten layer .
joinM :: (Monad m, Ghc.Base.Monad m) => m (m a) -> m a
joinM mmx = do 
            mx <- mmx
            x <- mx
            return x

--Monad laws
{-
Law 1)  return x >>= f = f x
        return x --> x is a type of a and return x is a type of m a
        f --> f is a type of a -> m b
        (>>=) --> (>>=) is a type of m a -> (a -> m b) -> m b
-}
{-
Law 2)  mx >>= return = mx (return is an identity for bind)
        mx --> mx is a type of m a
        return --> return is a type of a -> m a
        (>>=) --> (>>=) is a type of m a -> (a -> m b) -> m b
-}
{-
Law 3)  (m >>= f) >>= g = m >>= (\x -> f x >>= g)
        m --> m is a type of m a
        f --> f is a type of a -> m b
        g --> g is a type of b -> m c
        (>>=) --> (>>=) is a type of m a -> (a -> m b) -> m b (in a monadic certain the bind operator is associative)
-}

--Effectful programming
-- a -> b     -- pure function
-- a -> m b   -- effectful function 

{-
|------------------------------------|
|Type              |       Effect    |
|------------------|-----------------|
| a -> Maybe a     | Exceptions      |
| a -> [b]         | Non-determinism |
| a -> ST b        | Internal State  |
| a -> IO          | Input/Output    |
|------------------------------------|
-}

{-
What is the point of monads?
1) Support pure programming with effects
2) Use of monad is explicit in types
3) Can generalise function to any effect
-}

