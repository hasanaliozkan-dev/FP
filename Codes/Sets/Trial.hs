
module Trial where

import Prelude
import SetADT


add :: Eq a => a -> Set a -> Set a
add e s = insert e s