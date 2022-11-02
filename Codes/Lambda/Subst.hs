module Subst where

import Prelude


import Terms

import Data.IORef
import System.IO.Unsafe
import Data.Time.Format.ISO8601 (yearFormat)
import GHC.Generics (Generic1(to1))


counterRef :: IORef Int 
{-# NOINLINE counterRef #-}
counterRef = unsafePerformIO (newIORef 0)

incrementCount :: IO Int
incrementCount=  atomicModifyIORef counterRef (\c -> (c+1, c+1))

resetCount :: IO Int
resetCount = atomicModifyIORef counterRef (const (0, 0)) --(\c -> (0,0))


readCount :: IO Int
readCount = readIORef counterRef

freshVar :: String -> String
freshVar y =  let x = unsafePerformIO incrementCount in y ++ show x

find :: Eq a => a -> [a] -> Bool
find y l = 
    case l of 
        [] -> False
        x:xs -> if x == y then True else find y xs 
        --(x == y) || find y xs

uniqueH :: Eq a => [a] -> [a] -> [a]
uniqueH l acc=
    case l of 
        [] -> acc
        x:xs -> if find x acc then uniqueH xs acc else uniqueH xs (acc ++ [x])

unique :: Eq a => [a] -> [a]
unique l = uniqueH l []


fvH :: Term -> [String] -> [String]
fvH t acc =
    case t of 
        Var y       -> y:acc
        Lambda y t1 -> filter (\a -> a/= y) (fvH t1 acc)
        App t1 t2   -> unique (fvH t1 acc ++ fvH t2 acc)

fv :: Term -> [String]
fv t = fvH t []

inTerm :: String -> Term -> Bool
inTerm x t = 
    case t of 
        Var y       -> x == y
        Lambda y t1 -> if x == y then True else inTerm x t1 --(x == y) || inTerm x t1
        App t1 t2   -> inTerm x t1 || inTerm x t2