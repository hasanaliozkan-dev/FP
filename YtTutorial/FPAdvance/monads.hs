import Prelude hiding (Monad, (>>=), (>>), fail, return)

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
     