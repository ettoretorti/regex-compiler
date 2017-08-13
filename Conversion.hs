module Conversion (
    regexToDfa,
    regexToDfaWithMapping,
) where
import Regex
import DFA

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


allDrs :: (Eq a, Ord a) => Regex a -> [a] -> [(a, Regex a)]
allDrs rgx xs = [(x, derivative x rgx) | x <- xs]

acceptingMap m = Map.fromList [(y, nullable x) | (x, y) <- Map.toList m]

newRegexes existing candidates = Set.toList $ Set.fromList [c | c <- candidates, not $ c `Map.member` existing]

addTransitions idx to_ids trans pairs = trans `Map.union` Map.fromList [((idx, x), to_ids Map.! y) | (x, y) <- pairs]

dfaStep :: (Eq a, Ord a) => [a] -> Map.Map (Regex a) Int -> Map.Map (Int, a) Int -> Set.Set (Regex a) -> Set.Set (Regex a) -> Int -> (DFA a, Int -> Regex a)
dfaStep alphabet to_ids transitions to_visit visited count
  | Set.null to_visit = (DFA (length to_ids) alphabet (\s x -> transitions Map.! (s, x)) (\x -> (acceptingMap to_ids) Map.! x),
                         let inv = Map.fromList [(y, x) | (x, y) <- Map.toList to_ids] in (\s -> inv Map.! s))
  | otherwise         = let rgx  = Set.elemAt 0 to_visit
                            nset = Set.deleteAt 0 to_visit
                            idx  = to_ids Map.! rgx
                            visited' = visited `Set.union` Set.singleton rgx
                            ds   = allDrs rgx alphabet
                            ds_r = [y | (x, y) <- ds]
                            new_rgxs = newRegexes to_ids ds_r
                            count' = count + length new_rgxs
                            to_ids' = to_ids `Map.union` Map.fromList (zip new_rgxs [count..(count'-1)])
                            to_visit' = (rgx `Set.delete` to_visit) `Set.union` (Set.fromList new_rgxs `Set.difference` visited')
                            transitions' = addTransitions idx to_ids' transitions ds
                        in
                            dfaStep alphabet to_ids' transitions' to_visit' visited' count'
                            

regexToDfaWithMapping r a = dfaStep a (Map.singleton r 0) Map.empty (Set.singleton r) Set.empty 1

regexToDfa :: (Eq a, Ord a) => Regex a -> [a] -> DFA a
regexToDfa r a = fst $ dfaStep a (Map.singleton r 0) Map.empty (Set.singleton r) Set.empty 1
