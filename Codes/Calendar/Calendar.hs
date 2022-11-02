{-# LANGUAGE GADTs #-}

module Calendar where

import Prelude
import Aux
import Month
import Picture

pic :: Int -> Int -> String
pic t n = if 1 <= n && n <= t then show n else ""

daysOfMonth :: Month -> Year -> [Picture]
daysOfMonth m y =
    let (d, t) = monthInfo m y
    in myMap row (myMap (myRjustify 3) (myMap (pic t) [1-d..42-d]))

month :: Month -> Year -> Picture
month m y = tile (myGroupsOfSize 7 (daysOfMonth m y))




printMonth :: Month -> Year -> IO()
printMonth m y = 
    let len = 21 - length(combYM m y)
        mn = if even len then  beside (beside (blank 1 (len `div` 2) ) (row (combYM m y))) (blank 1 (len `div` 2))
             else  beside (beside (blank 1 (len `div` 2)) (row (combYM m y))) (blank 1 ((len `div` 2) + 1))
        weekdays = row " Su Mo Tu We Th Fr Sa"
    in printPic (above mn (above weekdays (month m y)))

prepareMonth :: Month -> Year -> Picture
prepareMonth m y = 
    let len = 21 - length(monthName m)
        mn = if even len then  beside (beside (blank 1 (len `div` 2) ) (row (monthName m ))) (blank 1 (len `div` 2))
             else  beside (beside (blank 1 (len `div` 2)) (row (monthName m ))) (blank 1 ((len `div` 2) + 1))
        weekdays = row " Su Mo Tu We Th Fr Sa"
    in above mn (above weekdays (month m y))

printYear :: Year -> IO()
printYear y =
    let l1 = [1..4]
        l2 = [5..8]
        l3 = [9..12]
        in printPic (above (beside (blank 1 46) (beside  (row (show y)) (blank 1 46))) (tile [recurse l1 y, recurse l2 y, recurse l3 y]))
        where
            recurse l y =
                case l of
                    []   -> []
                    x:xs -> beside (prepareMonth x y) (blank 8 3) : recurse xs y
