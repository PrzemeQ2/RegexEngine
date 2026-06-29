module Main (main) where
import System.Environment (getArgs)
import Parser
import AST
import Match
import DFA

checkMatch :: Bool -> IO ()
checkMatch isMatch = do 
    case isMatch of 
        True -> putStrLn "MATCH!"
        False -> putStrLn "NO MATCH!"

main :: IO ()
main = do
    args <- getArgs
    let isDfa = "--dfa" `elem` args
        rest = filter (/= "--dfa") args
    if length rest /= 2 
        then (do 
                putStrLn "Invalid number of arguments"  
                putStrLn "stack run -- \"(a|b)*c\" \"ababc\"")
        else 
            let pattern = rest !! 0 
                input = rest !! 1
                mr = parse pattern 
            in (do case mr of 
                    Nothing -> putStrLn ("Cannot parse pattern: " ++ pattern)
                    Just ast -> (do 
                        putStrLn ("pretty AST: " ++ pretty ast)
                        if isDfa 
                            then checkMatch $ matchDFA ast input 
                            else checkMatch $ matches ast input))