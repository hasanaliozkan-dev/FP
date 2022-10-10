{-# LANGUAGE GADTs #-}
import Prelude
import System.IO

mySum :: Num a => [a] -> a 
mySum [] = 0 
mySum (x:xs) = x + mySum xs 

mySumF :: Num a => [a] -> a 
mySumF = foldr (+) 0

myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop 0 xs = xs
myDrop n (_:xs) = myDrop (n-1) xs 

myRemoveLast :: [a] -> [a]
myRemoveLast (x:xs) | null xs   = []
                    | otherwise = x : myRemoveLast xs 

myRemoveLast1 :: [a] -> [a]
myRemoveLast1 [_] = []
myRemoveLast1 (x:xs) = x: myRemoveLast1 xs


data Answer = Yes | No | Unknown

answers :: [Answer]
answers = [Yes,No,Unknown]

flipAns :: Answer -> Answer
flipAns Yes = No
flipAns No = Yes
flipAns Unknown = Unknown 

data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi*r*r
area (Rect x y ) = x*y

perimeter :: Shape -> Float
perimeter (Circle r) = 2*pi*r
perimeter (Rect x y) = 2*x + 2*y 
 
data MyMaybe a = MyNothing | MyJust a



safediv :: Int -> Int -> MyMaybe Int
safediv _ 0 = MyNothing
safediv m n = MyJust (m `div` n)

safehead :: [a] -> MyMaybe a
safehead [] = MyNothing
safehead xs = MyJust (head xs) 


data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n 

int2nat :: Int -> Nat
int2nat  0 = Zero
int2nat n = Succ(int2nat (n-1))

addnat :: Nat -> Nat -> Nat
addnat  Zero  n = n
addnat (Succ m) n = Succ(addnat m n) 

mulnat :: Nat -> Nat -> Nat
mulnat Zero _ = Zero
mulnat _ Zero = Zero
mulnat (Succ n) m = addnat (mulnat n m ) m

{-data Expr = Val Int 
            |Add Expr Expr
            |Mul Expr Expr

-}


data Tree a = Leaf a | Node (Tree a) (Tree a)

data Op = Add| Sub | Mul | Div
myAplly :: Op -> Int -> Int -> Int
myAplly Add x y = x + y
myAplly Sub x y = x - y
myAplly Mul x y = x * y
myAplly Div x y = x `div` y

myValid :: Op -> Int -> Int -> Bool
myValid Add _ _ = True
myValid Sub x y = x>y
myValid Mul _ _ = True
myValid Div x y  = x `mod` y == 0

mySplit :: [a] -> [([a], [a])]
mySplit [_] = []
mySplit (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- mySplit xs]


act :: IO (Char,Char,Char)
act = do 
        x <- getChar
        y <- getChar
        z <- getChar
        return (x,y,z)

myGetLine :: IO String
myGetLine = do 
            x <- getChar
            if x == '\n' then
                return []
            else
                do
                    xs <- myGetLine
                    return (x:xs)

myPutStr :: String -> IO ()
myPutStr []     = return ()
myPutStr (x:xs) = do
                    putChar x
                    myPutStr xs  

myPutStrLn :: String -> IO ()
myPutStrLn xs = do
                  myPutStr xs
                  putChar '\n'
myStrLen :: IO ()
myStrLen = do
            myPutStr "Enter a string : "
            xs <- myGetLine
            myPutStr "The string has "
            myPutStr (show (length xs))
            myPutStrLn " characters."
{-

hangman :: IO()
hangman = do 
            myPutStrLn "Think of a word: "
            word <- sgetLine
            myPutStrLn "Try to guess it: "
            play word

sgetLine :: IO String
sgetLine do 
            x <- getCh
            if x == '\n' then
                do 
                    putChar x
                    return []
            else 
                do
                    putChar '-'
                    xs <- sgetLine
                    return (x:xs )

getCh :: IO Char
getCh = do
        hSetEcho stdin False
        x <- getChar
        hSetEcho stdin True
        return x

play :: String -> IO()
play word =
    do myPutStr "? "
    guess <- myGetLine
    if guess == word then
        myPutStrLn "You got it"
    else 
        do myPutStrLn (match word guess)
           play word 
-}

match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]


printStars :: Int -> IO()
printStars n = do 
                putChar '*'
                printStars (n-1) 

