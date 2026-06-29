module DFA where

import Match
import AST
import NFA
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

alphabet :: [(Int, Maybe Char, Int)] -> [Char] 
alphabet edges = Set.toList $ Set.fromList [c | (_, Just c, _)<-edges] 

transitionsFrom :: [(Int, Maybe Char, Int)] -> [Char] -> Set Int -> [(Char, Set Int)]
transitionsFrom edges alpha s = [(c, step edges s c) | c<-alpha]

explore :: [(Int, Maybe Char, Int)] -> [Char] -> [Set Int] -> Map (Set Int) [(Char, Set Int)]
explore edges alpha queue = go queue Map.empty
  where
    go [] m = m
    go (q:qs) m =
        let
            rqs = transitionsFrom edges alpha q
        in if Map.member q m 
            then go qs m
            else let 
                newMap = Map.insert q rqs m
                targets = [q' | (_, q') <- rqs]
                in go (qs ++ targets) newMap

data DFA = DFA 
    { dfaStart :: Set Int
    , dfaAccept :: [Set Int]
    , dfaEdges :: Map (Set Int) [(Char, Set Int)]
    } deriving (Show)

buildDFA :: Regex -> DFA 
buildDFA regex = 
    let nfa = compile regex
        edges = nfaEdges nfa
        accept = nfaAccept nfa
        alpha = alphabet edges
        startState = epsilonStep edges (Set.singleton $ (nfaStart nfa))
        m = explore edges alpha [startState]
        acceptStates = [k | k <- Map.keys m, accept `Set.member` k ]
    in DFA startState acceptStates m

stepDFA :: Map (Set Int) [(Char, Set Int)] -> Set Int -> Char -> Set Int
stepDFA edges qs c =
    case Map.lookup qs edges of 
        Nothing -> Set.empty
        Just transition -> 
            case lookup c transition of 
                Just qa -> qa
                Nothing -> Set.empty

matchDFA :: Regex -> String -> Bool
matchDFA regex input = 
    let dfa = buildDFA regex  
        qs = dfaStart dfa
        final = foldl (stepDFA (dfaEdges dfa)) qs input 
        in final `elem` (dfaAccept dfa)