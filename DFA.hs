
{-# LANGUAGE GADTs #-}

module DFA where

import Prelude
import Data.Set
import Data.Set.Internal

type State = Int

data DFA =
    DFA
    {
        dfa_state    :: Set State,
        dfa_alphabet :: Set Char,
        dfa_sigma    :: State -> Char -> State,
        dfa_start    :: State,
        dfa_finals   :: Set State
    }

-- your code here...



