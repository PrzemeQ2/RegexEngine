module DFA (DFA(..), alphabet, transitionsFrom, explore, buildDFA, stepDFA, matchDFA, minimizeDFA, matchMinDFA, runDFA) where

import Match ( epsilonStep, step )
import AST
import NFA
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)

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
                Just qn -> qn
                Nothing -> Set.empty

runDFA :: DFA -> String -> Bool
runDFA dfa input = foldl (stepDFA (dfaEdges dfa)) (dfaStart dfa) input `elem` dfaAccept dfa

matchDFA :: Regex -> String -> Bool
matchDFA regex input = runDFA (buildDFA regex) input


-- Minimization of DFA

alphabetDFA :: DFA -> [Char]
alphabetDFA dfa = Set.toList $ Set.fromList [ c | values <- Map.elems (dfaEdges dfa), (c, _) <- values]

isAcc :: DFA -> Set Int -> Bool
isAcc dfa k = k `elem` dfaAccept dfa

norm :: (Set Int, Set Int) -> (Set Int, Set Int)
norm (a,b) = if a <= b then (a,b) else (b,a)

basePairs :: DFA -> [(Set Int, Set Int)]
basePairs dfa = [ norm (k1, k2) | k1 <- ks, k2 <- ks, k1<k2, isAcc dfa k1 /= isAcc dfa k2]
    where
        ks = Map.keys (dfaEdges dfa)

distinguishable :: DFA -> [Char] -> Set (Set Int, Set Int)
distinguishable dfa alpha = go (Set.fromList base)
    where
        base = basePairs dfa
        ks = Map.keys (dfaEdges dfa)
        go pairs = 
            let new = [norm (a, b) | a <- ks, b <- ks, a < b
                        , not (Set.member (norm (a, b)) pairs)
                        , any (\c ->let da = stepDFA (dfaEdges dfa) a c 
                                        db = stepDFA (dfaEdges dfa) b c 
                                    in da /= db && (Set.member (norm(da, db)) pairs)) alpha]
                deltaPairs = Set.union pairs (Set.fromList new)
            in if Set.size deltaPairs == Set.size pairs then pairs else go deltaPairs

equiv :: Set (Set Int, Set Int) -> Set Int -> Set Int -> Bool
equiv dist p q = p == q || not (Set.member (norm (p, q)) dist)

rep :: Set (Set Int, Set Int) -> [Set Int] -> Set Int -> Set Int
rep dist allStates k = minimum [ s | s <- allStates, equiv dist k s] 

minimizeDFA :: DFA -> DFA
minimizeDFA dfa = DFA newStart newAccept newEdges 
    where
        alpha = alphabetDFA dfa
        dist = distinguishable dfa alpha
        allStates = Map.keys (dfaEdges dfa)
        r = rep dist allStates 
        newStart = r (dfaStart dfa)
        newAccept = Set.toList . Set.fromList $ map r (dfaAccept dfa)
        reps = Set.toList $ Set.fromList (map r allStates)
        newEdges = Map.fromList [(cr, edgesFor cr) | cr <- reps]
        edgesFor cr = [(c, r target) | (c, target) <- fromMaybe [] (Map.lookup cr (dfaEdges dfa))]


matchMinDFA :: Regex -> String -> Bool
matchMinDFA regex input = runDFA (minimizeDFA (buildDFA regex)) input