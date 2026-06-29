module AST (Regex(..), pretty) where 

data Regex =  Empty
            | Lit Char
            | Concat Regex Regex
            | Union Regex Regex
            | Star Regex 
            deriving (Eq, Show)

pretty :: Regex -> String 
pretty Empty = ""
pretty (Lit c) = [c]
pretty (Concat a b) = "(" ++ pretty a ++ pretty b ++ ")"
pretty (Union a b) = "(" ++ pretty a ++ "|"++ pretty b ++ ")"
pretty (Star a) = "(" ++ pretty a ++ ")*"