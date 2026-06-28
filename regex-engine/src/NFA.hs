module NFA where

import AST

import Control.Monad.State

getNewStateId :: State Int Int
getNewStateId= do 
    n <- get
    put (n+1)
    pure n

data NFA = NFA 
    { nfaStart  :: Int
    , nfaAccept :: Int
    , nfaEdges  :: [(Int, Maybe Char, Int)]
    } deriving (Show)

build :: Regex -> State Int NFA

build (Lit c) = do
    qs <- getNewStateId
    qa <- getNewStateId 
    pure (NFA qs qa [(qs, Just c, qa)]) 

build Empty = do 
    qs <- getNewStateId
    qa <- getNewStateId 
    pure (NFA qs qa [(qs, Nothing, qa)])

build (Concat r1 r2) = do
    q1 <- build r1
    q2 <- build r2
    let qs = nfaStart q1
        qa = nfaAccept q2
        e1 = nfaEdges q1
        e2 = nfaEdges q2
    pure (NFA qs qa 
        (e1 ++ e2 ++ [(nfaAccept q1, Nothing, nfaStart q2)]))

build (Union r1 r2) = do
    q1 <- build r1
    q2 <- build r2
    qs <- getNewStateId -- new start state
    qe <- getNewStateId -- new end state
    let e1 = nfaEdges q1
        e2 = nfaEdges q2
    pure (NFA qs qe 
        (e1 ++ e2 
                     ++ [(qs, Nothing, nfaStart q1)]
                     ++ [(qs, Nothing, nfaStart q2)]
                     ++ [(nfaAccept q1, Nothing, qe)]
                     ++ [(nfaAccept q2, Nothing, qe)]))

build (Star r) = do
    q <- build r
    qs <- getNewStateId
    qe <- getNewStateId 
    let skip = [(qs, Nothing, qe)]
        e = nfaEdges q
    pure (NFA qs qe 
            (skip 
                      ++ [(qs, Nothing, nfaStart q)]
                      ++ e
                      ++ [(nfaAccept q, Nothing, nfaStart q)]
                      ++ [(nfaAccept q, Nothing, qe)]))

compile :: Regex -> NFA
compile r = fst $ runState (build r) 0