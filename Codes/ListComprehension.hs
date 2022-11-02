module ListComprehension where

import Prelude

myLength :: [a] -> Int
myLength l = sum [1 | _ <-l]

myFirst :: [(a,b)] -> [a]
myFirst l = [x | (x,_) <- l]

myConcat  :: [[a]] -> [a]
myConcat l = [y | y:_ <- l]

