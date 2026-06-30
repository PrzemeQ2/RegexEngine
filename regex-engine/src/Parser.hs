module Parser (Parser, runParser, zero, item, (<|>), sat, char, many, many1, getPostfixOp, getUnionOp, regexP, termP, factorP, atomP, parse, parseWithErr, repeatConcat) where
import Control.Monad.State
import AST
import Data.Char (isDigit)

type Parser a = StateT String [] a

runParser :: Parser a -> String -> [(a, String)]
runParser = runStateT

-- Building blocks:

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
        <|> getBraceOp

getUnionOp :: Parser (Regex -> Regex -> Regex)
getUnionOp = char '|' >> pure Union

getBraceOp :: Parser (Regex -> Regex)
getBraceOp =
        (do _ <- char '{'
            n <- many1 (sat isDigit)
            _ <- char '}'
            let k = read n
            pure (\r -> repeatConcat k r))
    <|> (do _ <- char '{'
            n1 <- many1 (sat isDigit)
            _ <- char ','
            n2 <- many1 (sat isDigit)
            _ <- char '}'
            let k1 = read n1
                k2 = read n2
            if k2 >= k1
                then pure (\r -> Concat (repeatConcat k1 r) (repeatOpt (k2 - k1) r))
                else zero)
    <|> (do _ <- char '{'
            n <- many1 (sat isDigit)
            _ <- char ','
            _ <- char '}'
            let k = read n
            pure (\r -> Concat (repeatConcat k r) (Star r)))

repeatConcat :: Int -> Regex -> Regex 
repeatConcat 0 _ =  Empty
repeatConcat 1 reg = reg
repeatConcat n reg = (Concat reg (repeatConcat (n-1) reg))  

repeatOpt :: Int -> Regex -> Regex     
repeatOpt 0 _ = Empty
repeatOpt 1 reg = Union reg Empty
repeatOpt n reg = Concat (Union reg Empty) (repeatOpt (n-1) reg)

-- The grammar:
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

        metachars = "()|*+?{}\\"
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


parseWithErr :: String -> Either (Int, String) Regex
parseWithErr str =
    let list = runParser regexP str 
    in case list of 
        [] -> Left (1, "Cannot parse: " ++ str)  
        ((e, "") : _) -> Right e
        ((_, rest) : _) -> 
            let pos = length str - length rest
                errMsg = "Parse error at position " ++ show pos ++ ":\n"
                      ++ str ++ "\n"
                      ++ replicate pos ' ' ++ "^\n"
            in Left (pos, errMsg)