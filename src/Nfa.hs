module Nfa (isAccepted, NFA(NFAc), setTransition, State, Letter, makeComplete, times) where 
-- used because without we cannot apply 'show' to function
import Text.Show.Functions
import qualified Data.Set as Set--(Set, union, intersect, empty, fromList) 
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe
import Prelude hiding (init)

type State = Integer
type Letter = Char
errorState = -1

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

makeComplete :: NFA -> NFA
makeComplete (NFAc states alphabet init delta final) =
    let unassigned = (states `times` alphabet) `Set.difference` (Map.keysSet delta)
        init' = if init == Set.empty
            then Set.fromList [errorState]
            else init
    in let delta' = Map.fromList [(x,(Set.fromList[errorState])) | x <- (Set.toList unassigned)]
        in NFAc (states `Set.union` Set.fromList[errorState]) alphabet init' (delta `Map.union` delta') final

-- Normal cartesian product / settimes operation as it should exist in haskell...
times :: (Ord a, Ord b) => Set.Set a -> Set.Set b -> Set.Set (a,b)
times xs ys = Set.fromList[(x,y) | x <- (Set.toList xs), y <- (Set.toList ys)]
