module DFA (
    DFA(..),
    matches,
) where

-- state 0 is the initial state
data DFA a = DFA Int [a] (Int -> a -> Int) (Int -> Bool)

instance Show a => Show (DFA a) where
  show (DFA size alpha trans accept) = let accepting = show [x | x <- [0..(size-1)], accept x]
                                           alphas    = show alpha
                                           trans'    = show [[trans s a | a <- alpha] | s <- [0..(size-1)]]
                                       in  "size: " ++ show size ++ "\naccepting: " ++ accepting ++ "\nalphabet: " ++ alphas ++ "\n" ++ trans'

matches :: [a] -> DFA a -> Bool
matches str (DFA _ _ trans accept) = accept $ foldl trans 0 str
