module Month where

import Aux

type Year    = Int
type Month   = Int 
type Dayname = Int -- 0 Sunday, 1 Monday, .., 6 Saturday


leap :: Year -> Bool
leap y = if mod y 100 == 0 then mod y 400 == 0 else mod y 4 == 0

january1 :: Year -> Dayname
january1 y =
    let x = y - 1
    in mod (x * 365 + div x 4 - div x 100 + div x 400 + 1) 7

monthLengths :: Year -> [Int]
monthLengths y =
    let feb = if leap y then 29 else 28
    in [31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

monthName :: Month -> String
monthName m = ["January","February","March","April","May","June","July","August","September","October","November","December"]!! (m-1)

combYM :: Month -> Year -> String
combYM m y = monthName m ++ " " ++ show y

firstDaysOfMonths :: Year -> [Dayname]
firstDaysOfMonths y =
    let fd = january1 y
        ml = monthLengths y
    in myMap (`mod` 7) (initialDays ml fd)
    where
        initialDays ml fd =
            case ml of
                []   -> []
                x:xs -> fd : initialDays xs (fd + x)

monthInfo :: Month -> Year -> (Dayname, Int)
monthInfo m y =
    let fd = firstDaysOfMonths y 
        ml = monthLengths y
    in myZip fd ml !! (m-1)