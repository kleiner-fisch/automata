module Nfa (isAccepted, NFA(NFAc), setTransition, State, Letter, makeComplete, times) where 
-- used because without we cannot apply 'show' to function
import Text.Show.Functions
import qualified Data.Set as Set 
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe
import Prelude hiding (init)

type State = Integer
type Letter = Char
errorState = -1
errorSet = Set.singleton errorState

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

-- makes an automaton complete s.t. for each pair in (States x Alphabet) a the transition function returns a state.
-- For this a sink state is added to States which is the result of all previously unassigned pairs in (States x Alphabet).
-- This function keeps DFA deterministc. It adds the sinkstate, but it will be unreachable.
makeComplete :: NFA -> NFA
makeComplete (NFAc states alphabet init delta final) =
    let unassigned = (states `times` alphabet) `Set.difference` (Map.keysSet delta)
    in let 
        unassigned' = unassigned `Set.union` (errorSet `times` alphabet)
        init' = if init == Set.empty
            then errorSet
            else init
        in let delta' = Map.fromList [(x,errorSet) | x <- (Set.toList unassigned')]
            in NFAc (states `Set.union` errorSet) alphabet init' (delta `Map.union` delta') final

-- Creates an automaton that accepts the union of the languages accepted by the input automata.
union :: NFA -> NFA -> NFA

-- Creates an automaton that accepts the intersection of the languages accepted by the input automata.
intersection :: NFA -> NFA -> NFA

-- Creates an automaton that accepts the complement of the input NFA
complement :: NFA -> NFA

-- Normal cartesian product / settimes operation as it should exist in haskell...
times :: (Ord a, Ord b) => Set.Set a -> Set.Set b -> Set.Set (a,b)
times xs ys = Set.fromList[(x,y) | x <- (Set.toList xs), y <- (Set.toList ys)]
