import Nfa
import Data.Set
 
states = fromList[1,2,3]
alphabet = fromList['a','b']
init = fromList[1]
final = fromList[2]

delta :: State -> Letter -> (Set State)
delta 1 'a' = fromList[1]
delta 1 'b' = fromList[2]
delta 2 _ = fromList[3]
delta 3 _ = fromList[3]
