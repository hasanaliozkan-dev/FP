{-# LANGUAGE GADTs #-}

module BinTree where

import Prelude

data BinTree a where
    Empty :: BinTree a
    Node  :: a -> BinTree a -> BinTree a -> BinTree a

printBinTree :: Show a => BinTree a -> String
printBinTree t =
    case t of
        Empty      -> "_"
        Node n l r -> "[" ++ show n ++ "," ++ printBinTree l ++ "," ++ printBinTree r ++ "]" 

instance Show a => Show (BinTree a) where
    show t = printBinTree t