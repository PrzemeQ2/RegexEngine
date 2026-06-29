module Match (charStep, epsilonStep, step, matches) where

import AST
import NFA
import qualified Data.Set as Set
import Data.Set (Set)

charStep :: [(Int, Maybe Char, Int)] -> Char -> Set Int -> Set Int
charStep edges c current = Set.fromList [to | (from, Just c', to) <- edges
                                        , c' == c
                                        , from `Set.member` current]

epsilonStep :: [(Int, Maybe Char, Int)] -> Set Int -> Set Int
epsilonStep edges start = go start 
    where 
        go current = 
            let reachable = Set.fromList [to | (from, Nothing, to) <- edges, from `Set.member` current]
                next = Set.union current reachable 
            in if current == next then current else go next

step :: [(Int, Maybe Char, Int)] -> Set Int -> Char -> Set Int
step edges current c = epsilonStep edges (charStep edges c current)

matches :: Regex -> String -> Bool
matches regex input = 
    let nfa = compile regex
        edges = nfaEdges nfa
        start = epsilonStep edges (Set.singleton $ nfaStart nfa)
        final = foldl (step edges) start input 
    in Set.member (nfaAccept nfa) final
