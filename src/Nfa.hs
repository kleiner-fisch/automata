module Nfa (isAccepted, NFA(NFAc), setTransition, State, Letter) where 
-- used because without we cannot apply 'show' to function
import Text.Show.Functions
import qualified Data.Set as Set--(Set, union, intersect, empty, fromList) 
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe

type State = Integer
type Letter = Char
--type Word = String

-- (S, Sigma, I, delta, F)
data NFA = NFAc (Set.Set State)  (Set.Set Letter) (Set.Set State)  (Map.Map (State, Letter)  (Set.Set State)) (Set.Set State)
        deriving (Show)

-- Advance all states in the set by one step, for the given input letter 
setTransition :: (Map.Map (State, Letter) (Set.Set State))  -> (Set.Set State) -> Letter -> (Set.Set State)
setTransition delta xs sigma    = Set.fold f Set.empty xs
                where f q ys = (fromJust (Map.lookup (q, sigma) delta)) `Set.union` ys
 
-- naivly test whether a given word is accepted
-- for this we forward propagate the current state sets on our input word
-- we assume the automaton is complete
isAccepted :: NFA -> [Letter] -> Maybe Bool
isAccepted (NFAc states alphabet init delta final) word 
    = if ((Set.fromList word) `Set.union` alphabet) /= alphabet
          then Nothing
          else let f xs sigma = setTransition delta xs sigma
            in Just (((List.foldl f init word) `Set.intersection` final) /= Set.empty)

--makeComplete :: NFA -> NFA
--makeComplete (NFAc states alphabet init delta final) = 
