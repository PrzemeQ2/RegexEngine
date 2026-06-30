module Main (main) where
import System.Environment (getArgs)
import Parser
import AST
import Match
import DFA
import qualified Data.Map as Map

checkMatch :: Bool -> IO ()
checkMatch isMatch = do
    case isMatch of
        True  -> putStrLn "MATCH!"
        False -> putStrLn "NO MATCH!"

showMinComparison :: Regex -> IO ()
showMinComparison ast = do
    let dfa    = buildDFA ast
        minDfa = minimizeDFA dfa
        before = Map.size (dfaEdges dfa)
        after  = Map.size (dfaEdges minDfa)
    putStrLn "DFA size comparison:"
    putStrLn ("  subset construction : " ++ show before ++ " states")
    putStrLn ("  after minimization  : " ++ show after  ++ " states")
    putStrLn ("  reduced by          : " ++ show (before - after) ++ " states")

main :: IO ()
main = do
    args <- getArgs
    let useDfa = "--dfa" `elem` args
        useMin = "--min" `elem` args
        rest   = filter (`notElem` ["--dfa", "--min"]) args
    if length rest /= 2
        then do
            putStrLn "Invalid number of arguments"
            putStrLn "Ex: stack run -- \"(a|b)*c\" \"ababc\"          (NFA engine)"
            putStrLn "    stack run -- --dfa \"(a|b)*c\" \"ababc\"   (DFA engine)"
            putStrLn "    stack run -- --min \"(a|b)*c\" \"ababc\"   (minimized DFA + size comparison)"
        else do
            let pattern = rest !! 0
                input   = rest !! 1
            case parseWithErr pattern of
                Left (_, errMsg) -> putStr errMsg
                Right ast -> do
                    putStrLn ("pretty AST: " ++ pretty ast)
                    if useMin
                        then do
                            showMinComparison ast
                            checkMatch (matchMinDFA ast input)
                        else if useDfa
                            then checkMatch (matchDFA ast input)
                            else checkMatch (matches ast input)