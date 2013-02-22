import Nfa
import qualified Data.Set as Set
import qualified Data.Map as Map
import Prelude hiding (init)

states = Set.fromList[1,2,3]
alphabet = Set.fromList['a','b']
init = Set.fromList[1]
final = Set.fromList[2]
a1 = NFAc states alphabet init delta final
a2 = makeComplete a1

delta = Map.fromList[((1,'a'),Set.fromList[1]),
    ((1,'b'),Set.fromList[2])]
--    ((2,'a'),Set.fromList[3]),
--    ((2,'b'),Set.fromList[3]),
--    ((3,'a'),Set.fromList[3]),
--    ((3,'b'),Set.fromList[3])]
