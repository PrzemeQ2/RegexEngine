module AST () where 

data Regex =  Empty
            | Lit Char
            | Concat Regex Regex
            | Union Regex Regex
            | Star Regex Regex
            deriving (Eq, Show)

