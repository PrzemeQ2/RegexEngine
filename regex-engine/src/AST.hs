module AST where 

import Control.Monad.State
data Regex =  Empty
            | Lit Char
            | Concat Regex Regex
            | Union Regex Regex
            | Star Regex 
            deriving (Eq, Show)

type Parser a = StateT String [] a

runParser :: Parser a -> String -> [(a, String)]
runParser = runStateT

-- Building blocks

zero :: Parser a
zero = StateT (const [])

item :: Parser Char 
item = do 
    s <- get 
    case s of 
        c:cs -> put cs >> pure c 
        [] -> zero

infixr 5 <|>
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = StateT $ \s -> 
    case runStateT p s of 
        [] -> runStateT q s 
        rs -> rs

sat :: (Char -> Bool) -> Parser Char 
sat predicate = do 
    c <- item 
    if predicate c then pure c else zero

char :: Char -> Parser Char 
char c = sat (==c)

many :: Parser a -> Parser [a]
many p = many1 p <|> pure []

many1 :: Parser a -> Parser [a]
many1 p = do
    x <- p
    xs <- many p
    pure (x:xs) 

getPostfixOp :: Parser (Regex -> Regex)
getPostfixOp = (char '*' >> pure (\r -> Star r)) 
        <|> (char '+' >> pure (\r -> Concat r (Star r)))
        <|> (char '?' >> pure (\r -> Union r Empty))

getUnionOp :: Parser (Regex -> Regex -> Regex)
getUnionOp = char '|' >> pure Union

-- The grammar
-- regex  ::= term ('|' term)*
-- term   ::= factor*
-- factor ::= atom ('*' | '+' | '?')*
-- atom   ::= '(' regex ')'  |  '\' char  | literal

regexP, termP, factorP, atomP :: Parser Regex

atomP = parenthesisP <|> metacharP <|> litP
    where
        parenthesisP = do 
            _ <- char '('
            r <- regexP
            _ <- char ')'
            pure r
        
        metacharP = do 
            _ <- char '\\'
            c <- item 
            pure (Lit c)

        metachars = "()|*+?\\"
        litP = do 
            c <- sat (`notElem` metachars)
            pure (Lit c)

factorP = do  
    a <- atomP
    rest a
    where 
        rest a =
            (do {op <- getPostfixOp ; rest( op a )})
            <|> pure a

termP = do
    fs <- many factorP 
    pure (combine fs)
    where 
        combine [] = Empty
        combine [x] = x
        combine (x:xs) = Concat x (combine xs)   

regexP = do 
    t <- termP
    rest t
        where
            rest t1 = 
                (do f <- getUnionOp 
                    t2 <- termP
                    rest (f t1 t2))
                <|> pure t1


parse :: String -> Maybe Regex
parse str = case [ e | (e, "") <- runParser regexP str ] of 
            ( e : _) -> Just e
            _  -> Nothing

pretty :: Regex -> String 
pretty Empty = ""
pretty (Lit c) = [c]
pretty (Concat a b) = "(" ++ pretty a ++ pretty b ++ ")"
pretty (Union a b) = "(" ++ pretty a ++ "|"++ pretty b ++ ")"
pretty (Star a) = "(" ++ pretty a ++ ")*"