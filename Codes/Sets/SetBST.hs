module SetBST where



import BTree

import Operations

data Set a where 
    Cons :: BTree a -> Set a

setBST2String :: Show a => Set a -> String

setBST2String s = 
            case s of 
                Cons t -> let ft = flatten t
                            in "{" ++list2String ft ++ "}" 
                            where 
                                list2String l = 
                                    case l of 
                                        [] -> ""
                                        [e] -> show e
                                        x:xs -> show x ++ ", " ++ list2String xs

instance Show a => Show (Set a ) where 
    show s = setBST2String 
