module Nfa (isAccepted, NFA(NFAc), setTransition, makeComplete, times) where 
-- used because without we cannot apply 'show' to function
import Text.Show.Functions
import qualified Data.Set as Set 
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe
import Prelude hiding (init)

errorState = -1
errorSet = Set.singleton errorState

-- a is the type for the states. b is the type for the alphabet.
-- Meaning of the parameters are (S, Sigma, I, delta, F)
data (Eq state, Eq letter) => NFA state letter = NFAc (Set.Set state)  (Set.Set letter) (Set.Set state)  (Map.Map (state, letter)  (Set.Set state)) (Set.Set state)
        deriving (Show)

-- Advance all states in the set by one step, for the given input letter 
setTransition :: (Ord state, Ord letter) => (Map.Map (state, letter) (Set.Set state))  -> (Set.Set state) -> letter -> (Set.Set state)
setTransition delta xs sigma    = Set.fold f Set.empty xs
                where f q ys = (fromJust (Map.lookup (q, sigma) delta)) `Set.union` ys
 
-- naivly test whether a given word is accepted
-- for this we forward propagate the current state sets on our input word
-- we assume the automaton is complete
isAccepted :: (Ord a) => (Ord b) => (Eq b) => NFA a b -> [b] -> Maybe Bool
isAccepted (NFAc states alphabet init delta final) word 
    = if ((Set.fromList word) `Set.union` alphabet) /= alphabet
          then Nothing
          else let f xs sigma = setTransition delta xs sigma
            in Just (((List.foldl f init word) `Set.intersection` final) /= Set.empty)

-- makes an automaton complete s.t. for each pair in (States x Alphabet) a the transition function returns a state.
-- For this a sink state is added to States which is the result of all previously unassigned pairs in (States x Alphabet).
-- This function keeps DFA deterministc. It adds the sinkstate, but it will be unreachable.
--makeComplete :: NFA -> NFA
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
-- we assume the input automata are complete
--union :: NFA -> NFA -> Maybe NFA
union (NFAc states1 alphabet1 init1 delta1 final1) (NFAc states2 alphabet2 init2 delta2 final2) =
    if alphabet1 == alphabet2
        then Just (NFAc states3 alphabet1 init3 delta3 final3) 
        else Nothing
    where states3   = states1 `times` states2
          init3       = init1 `times` init2
          final3      = final1 `times` final2
          delta3      = deltaUnion delta1 delta2

-- Gets the set of toStates for given input letter and fromState
deltaLookup :: (Ord a, Ord b) => a -> b -> Map.Map (a,b) (Set.Set a) -> Set.Set a
deltaLookup s l delta = if Map.lookup (s,l) delta == Nothing
                            then Set.empty
                            else fromJust (Map.lookup (s,l) delta)

-- Creates the union of the two transition-maps, syncronized on the common alphabet. The alphabet is assumed to be common.
deltaUnion :: (Ord b, Eq a1, Eq a2) =>  (Ord a1, Ord a2) => Map.Map (a1, b) (Set.Set a1) -> Map.Map (a2, b) (Set.Set a2) -> Map.Map ((a1, a2), b) (Set.Set (a1, a2))
deltaUnion delta1 delta2 = Map.fromList [(((s1,s2),l1), (to1 `times` to2)) | ((s1,l1),to1) <- Map.toList delta1, ((s2,l2),to2) <- Map.toList delta2, l1 == l2]

-- Creates an automaton that accepts the intersection of the languages accepted by the input automata.
-- intersection :: NFA -> NFA -> NFA

-- Creates an automaton that accepts the complement of the input NFA
-- complement :: NFA -> NFA

-- Normal cartesian product / settimes operation as it should exist in haskell...
times :: (Ord a, Ord b) => Set.Set a -> Set.Set b -> Set.Set (a,b)
times xs ys = Set.fromList[(x,y) | x <- (Set.toList xs), y <- (Set.toList ys)]
