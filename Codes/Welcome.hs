module Welcome where

import Prelude
--empty pair () is unit
main :: IO()
main = do   
            putStrLn "What is your name?" 
            name <- getLine
            putStrLn ("Pleased to meet you "  ++ name ++ ".")

intSquare :: IO()
intSquare = do 
                putStr "Enter an integer to square: "
                i <- getLine
                let j = (read i :: Int) 
                        in putStrLn ("The square of " ++ show j ++" is "++ show (j^2))


intIOAdd :: IO Int -> IO Int -> IO Int
intIOAdd n m= 
    do 
        i<- n
        j <- m
        return (i + j)