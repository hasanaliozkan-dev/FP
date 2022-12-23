{-# LANGUAGE GADTs #-}

module BTree(BTree(..), show, printBTree) where

import Prelude

data BTree a where
    Empty :: BTree a
    Node  :: a -> BTree a -> BTree a -> BTree a

printBTree :: Show a => BTree a -> String
printBTree t =
    case t of
        Empty      -> "_"
        Node n l r -> "[" ++ show n ++ "," ++ printBTree l ++ "," ++ printBTree r ++ "]" 

instance Show a => Show (BTree a) where
    show t = printBTree t