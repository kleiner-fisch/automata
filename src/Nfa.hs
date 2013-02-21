module Nfa (isAccepted, NFA(NFAc), setTransition, State, Letter) where 
-- used because without we cannot apply 'show' to function
import Text.Show.Functions
import Data.Set --(Set, union, intersect, empty, fromList) 

type State = Integer
type Letter = Char
--type Word = String

-- (S, Sigma, I, delta, F)
data NFA = NFAc (Set State)  (Set Letter) (Set State)  (State -> Letter -> (Set State)) (Set State)
        deriving (Show)

setTransition :: (State -> Letter -> (Set State))  -> (Set State) -> Letter -> (Set State)
setTransition delta xs sigma    = fold f empty xs
                where f q ys = (delta q sigma) `union` ys
 
-- naivly test whether a given word is accepted
-- for this we forward propagate the current state sets on our input word
-- we assume the automaton is complete
isAccepted :: NFA -> [Letter] -> Maybe Bool
isAccepted (NFAc states alphabet init delta final) word 
    = if ((fromList word) `union` alphabet) /= alphabet
          then Nothing
          else let f xs sigma = setTransition delta xs sigma
            in Just (((foldl f init word) `intersection` final) /= empty)

