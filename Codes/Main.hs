module Main where

import Prelude
import System.Environment


foreach :: [a] -> (a -> IO()) -> IO()
foreach l f = 
    do 
        case l of 
            [] -> return ()
            x:xs -> 
                do 
                    f x 
                    foreach xs f


readAndPrint :: String -> IO()
readAndPrint s = 
    do 
        file <- readFile  s
        putStrLn file


main :: IO()
main = 
    do 
        files <- getArgs
        case files of 
            [] -> interact id
            x:xs -> foreach(x:xs) readAndPrint








{-

main :: IO()
main = 
    do 
        [file] <- getArgs
        s <- readFile file
        putStrLn (s ++ "\n")



count :: String -> String
count s = 
    "#l: " ++ show(length (lines s)) ++ " " ++
    "#w: " ++ show(length (words s)) ++ " " ++
    "#c: " ++ show(length  s) ++ ". \n" 
    
main :: IO()
main =  interact count -}