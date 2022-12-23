
{-# LANGUAGE GADTs #-}
-- I write getDFA1 and getDFA2 functions to get the DFA's easily.
-- I also implemented the print functions for each element of the quintuple in order to see the DFA clearly.
-- The print functions are bottom of the page.

module DFAHasanAliOZKAN where

import Prelude
import Data.Set

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

getDFA1 :: DFA
getDFA1 = DFA (fromList [0..5]) (fromList ['a','b']) sigma1 0 (fromList [5])

getDFA2 :: DFA
getDFA2 = DFA (fromList [0..13]) (fromList ['a']) sigma2 0 (fromList [0,2,4,6,7,8,10,12])

sigma1:: State -> Char -> State
sigma1 s c = 
    case s of 
        s -> if s == 0 || s == 1 || s == 3 then
                case c of
                    'a' -> s+1
                    'b' -> 0
            else if s == 2 || s == 4 then
                case c of 
                    'a' -> 2
                    'b' -> s+1
            else if s == 5 then 5
            else error  "State not defined"

sigma2:: State -> Char -> State
sigma2 s 'a' = 
    case s of 
        13 -> 0
        s -> if s < 13 && s >= 0 then s + 1 
        else error "State not defined"

printTransitionFunctionH :: DFA  -> [(State, Char, State)]
printTransitionFunctionH (DFA states alphabet sigma start finals) = 
    [(s, a, sigma s a) | s <- toList states, a <- toList alphabet]

printTransitionFunction :: DFA -> String
printTransitionFunction dfa = 
    unlines ( Prelude.map (\(s, a, s') -> show s ++ "\t -> " ++ show a ++ "\t -> " ++ show s') $ printTransitionFunctionH dfa)

{-
dfa1 :: State-> Char -> State
dfa1 0 'a' = 1
dfa1 0 'b' = 0
dfa1 1 'a' = 2
dfa1 1 'b' = 0
dfa1 2 'a' = 2
dfa1 2 'b' = 3
dfa1 3 'a' = 4
dfa1 3 'b' = 0
dfa1 4 'a' = 2
dfa1 4 'b' = 5
dfa1 5  _  = 5
dfa1 _  _  = error "State not defined"
-}

dfa_multiStep :: DFA -> State -> String -> State
dfa_multiStep dfa state s = 
                        case s of
                            [] -> state
                            (x:xs) -> dfa_multiStep dfa (dfa_sigma dfa state x) xs 

dfa_acceptance :: DFA -> String -> Bool
dfa_acceptance dfa str = member (dfa_multiStep dfa (dfa_start dfa) str) (dfa_finals dfa)

printStates :: DFA -> String
printStates (DFA states alphabet sigma start finals) = show (toList states)

printAlphabet :: DFA -> String
printAlphabet (DFA states alphabet sigma start finals) = show (toList alphabet)

printFinals :: DFA -> String
printFinals (DFA states alphabet sigma start finals) = show (toList finals)


instance Show DFA where
    show (DFA states alphabet sigma start finals) =
        "--------------------------------------------" ++
        "\nDeterministic Finite State Automaton" 
        ++ "\nStates: " ++ printStates (DFA states alphabet sigma start finals)
        ++ "\nAplhabet: " ++ printAlphabet (DFA states alphabet sigma start finals) 
        ++ "\nStart State: " ++show start 
        ++ "\nFinal States: " ++printFinals (DFA states alphabet sigma start finals)
        ++ "\nTransition Function: \n"  ++ printTransitionFunction (DFA states alphabet sigma start finals) ++
        "--------------------------------------------"