

--Monad is used for seperate pure and impure functions each other. Pure -> Mathematical, Functions Impure ->   IO functions


module Picture where

import Aux ( myFoldr1, myMap, myZipWith )
import Month
import Prelude

type Height  = Int
type Width   = Int
type Picture = (Height, Width, [[Char]])

printPic :: Picture -> IO()
printPic p =
    case p of
        (h, w, l) -> putStr (unlines l)

height :: Picture -> Int
height p =
    case p of
        (h, w, l) -> h

width :: Picture -> Int
width p =
    case p of
        (h, w, l) -> w

pixel :: Char -> Picture
pixel c = (1, 1, [[c]])

row :: String -> Picture
row s = (1, length s, [s])

blank :: Int -> Int -> Picture
blank h w = (h, w, replicate h (replicate w ' '))

above :: Picture -> Picture -> Picture
above p q =
    case p of
        (h1, w1, l1) ->
            case q of
                (h2, w2, l2) ->
                    if w1 == w2 
                    then (h1 + h2, w1, l1 ++ l2) 
                    else error "pics with different widths"

stack :: [Picture] -> Picture
stack l = myFoldr1 (above) l

beside :: Picture -> Picture -> Picture
beside p q =
    case p of
    (h1, w1, l1) ->
        case q of
            (h2, w2, l2) ->
                if h1 == h2 
                then (h1, w1 + w2, myZipWith (++) l1 l2) 
                else error "pics with different heights"

spread :: [Picture] -> Picture
spread l = myFoldr1 (beside) l

tile :: [[Picture]] -> Picture
tile l = stack (myMap (spread) l)

