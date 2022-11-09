
{-# LANGUAGE GADTs #-} -- do not remove or modify this line

-- I have implemented all functionalities on Rat and Pos data types. I have also add some extra functions
-- such as "printPosBin" and "ratSimplify" methods
--printPosBin prints out the Pos data type as binary form e.g. 4 = 100
--ratSimplify method simplify the Rat data type by using gcd function from prelude (I don't use this funtion any arithmetic operations on Rat data type)
   
import Prelude

{- Positive Integers -}

data Pos where
    XI :: Pos -> Pos
    XO :: Pos -> Pos
    XH :: Pos
    
pos2int :: Pos -> Int
pos2int p = 
    case p of 
        XH -> 1
        XO p -> 2 * pos2int p
        XI p -> 2 * pos2int p + 1

printPos :: Pos -> String
printPos p = 
    case p of 
        XH   -> "XH"
        XI k -> "XI (" ++ printPos k ++ ")"
        XO k -> "XO (" ++ printPos k ++ ")"


printPosBin :: Pos -> String
printPosBin p = 
    case p of 
        XH   -> "1" 
        XI k ->  printPosBin k ++ "1"
        XO k ->  printPosBin k ++ "0" 


int2pos :: Int -> Pos
int2pos n =  
    case n of
        0 -> error  "Zero(0) is not a positive number please try again..."
        1 -> XH
        n -> if even n then XO (int2pos (n `div` 2)) 
            else XI (int2pos (n `div` 2))

instance Show Pos where
    show p =  printPos p

posEq :: Pos -> Pos -> Bool
posEq p1 p2 = pos2int p1 == pos2int p2

instance Eq Pos where
    p == q = posEq p q
        
posLeq :: Pos -> Pos -> Bool
posLeq p1 p2 = pos2int p1 <= pos2int p2

instance Ord Pos where
    p <= q = posLeq p q

posAdd :: Pos-> Pos -> Pos 
posAdd p1 p2 = int2pos (pos2int p1 + pos2int p2)

posMult :: Pos-> Pos -> Pos 
posMult p1 p2 = int2pos (pos2int p1 * pos2int p2)

posSubtr :: Pos -> Pos -> Pos 
posSubtr p1 p2 = if p1 >= p2 then int2pos (pos2int p1 - pos2int p2)
                 else int2pos (pos2int p2 - pos2int p1)

posDiv :: Pos -> Pos -> Pos 
posDiv p1 p2 = int2pos(pos2int p1 `div` pos2int p2)

posAbs :: Pos -> Pos 
posAbs p = p

posSignum :: Pos -> Pos 
posSignum p = if pos2int p <= 0 then error "Zero(0) is not a positive number please try again..." else XH

posFromInteger :: Integer -> Pos
posFromInteger i =  if i <= 0 then XH 
                    else case i of 
                        1 -> XH
                        i -> if even i then XO (posFromInteger (i `div` 2))
                        else XI (posFromInteger(i `div` 2))

instance Num Pos where
    n + m = posAdd n m
    n - m = posSubtr n m
    n * m = posMult n m
    abs n = posAbs n
    signum  n = posSignum n
    fromInteger n = posFromInteger n 

{- Rational Numbers -}

data Rat where
    Frac :: Int -> Pos -> Rat

printRat ::Rat-> String
printRat (Frac i p) =
    case (i ,p)of 
        --(0 _) -> "0/1"
        (0 , p) -> "0" ++ "/" ++ show (pos2int p)
        (i, p) -> show i ++ "/" ++ show (pos2int p)

instance Show Rat where
    show r = show (printRat r)

ratSimplify :: Rat -> Rat
ratSimplify (Frac i p) =    if gcd i (pos2int p) /= 1 then 
                            Frac (i `div` gcd i (pos2int p)) (int2pos (pos2int p `div` gcd i (pos2int p)))
                            else Frac i p


ratEq :: Rat -> Rat -> Bool
ratEq (Frac i1 p1) (Frac i2 p2) = i1* pos2int p2 == i2* pos2int p1

instance Eq Rat where
    (Frac i1 p1 ) == (Frac i2 p2) = ratEq (Frac i1 p1) (Frac i2 p2)

ratLeq :: Rat -> Rat -> Bool
ratLeq (Frac i1 p1) (Frac i2 p2) =  if p1 == p2 then i1 <=i2 
                                    else i1* pos2int p2 <= i2* pos2int p1  

instance Ord Rat where
    (Frac i1 p1 ) <= (Frac i2 p2) = ratLeq (Frac i1 p1) (Frac i2 p2)

ratAdd :: Rat -> Rat -> Rat
ratAdd (Frac i1 p1) (Frac i2 p2) =  if p1 == p2 then Frac (i1+i2) p1
                                    else Frac ((i1 * div (lcm (pos2int p1) (pos2int p2)) (pos2int p1)) + (i2 * div (lcm (pos2int p1) (pos2int p2)) (pos2int p2))) (int2pos (lcm (pos2int p1) (pos2int p2)))


ratSubtr :: Rat -> Rat -> Rat
ratSubtr (Frac i1 p1) (Frac i2 p2)= if p1 == p2 then Frac (i1-i2) p1                                    
                                    else Frac ((i1 * div (lcm (pos2int p1) (pos2int p2)) (pos2int p1)) - (i2 * div (lcm (pos2int p1) (pos2int p2)) (pos2int p2))) (int2pos (lcm (pos2int p1) (pos2int p2)))

ratMult :: Rat -> Rat -> Rat
ratMult (Frac i1 p1) (Frac i2 p2) = Frac (i1*i2) (p1*p2)

ratSignum :: Rat -> Rat
ratSignum (Frac i p)
  | i < 0 = Frac (- 1) XH
  | i == 0 = Frac 0 XH
  | otherwise = Frac 1 XH


--ratSignum (Frac i p) = if i < 0 then Frac (-1) XH else if i == 0 then Frac 0 XH else Frac 1 XH

ratAbs :: Rat-> Rat
ratAbs (Frac i p) = if i < 0 then Frac (-i) p
                    else Frac i p

ratFromInteger :: Integer -> Rat 
ratFromInteger i = Frac (fromIntegral i) XH

instance Num Rat where
    (Frac i1 p1) + (Frac i2 p2) = ratAdd (Frac i1 p1) (Frac i2 p2)
    (Frac i1 p1) - (Frac i2 p2) = ratSubtr (Frac i1 p1) (Frac i2 p2)
    (Frac i1 p1) * (Frac i2 p2) = ratMult (Frac i1 p1) (Frac i2 p2)
    abs (Frac i p) = ratAbs (Frac i p)
    signum (Frac i p) = ratSignum(Frac i p)
    fromInteger n = ratFromInteger n 
