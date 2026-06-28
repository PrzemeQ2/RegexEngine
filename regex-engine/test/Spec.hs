import Test.Hspec
import AST
import NFA
import Match
import Data.Maybe (fromJust)

m :: String -> String -> Bool
m pattern input = matches (fromJust (parse pattern)) input

main :: IO ()
main = hspec $ do 
    describe "AST Parser tests:" $ do
        it "parse: \"a\" Just ( Lit 'a')" $ 
            parse "a" `shouldBe` Just ( Lit 'a')
        it "parse: \"abc\" Just (Concat (Lit 'a') (Concat (Lit 'b') (Lit 'c')))" $ 
            parse "abc" `shouldBe` Just (Concat (Lit 'a') (Concat (Lit 'b') (Lit 'c')))
        it "parse: \"a|b\" Just (Union (Lit 'a') (Lit 'b'))" $ 
            parse "a|b" `shouldBe` Just (Union (Lit 'a') (Lit 'b'))
        it "parse: \"a|b|c\" Just (Union (Union (Lit 'a') (Lit 'b')) (Lit 'c'))" $ 
            parse "a|b|c" `shouldBe` Just (Union (Union (Lit 'a') (Lit 'b')) (Lit 'c'))
        it "parse: \"ab|c\" Just (Union (Concat (Lit 'a') (Lit 'b')) (Lit 'c'))" $ 
            parse "ab|c" `shouldBe` Just (Union (Concat (Lit 'a') (Lit 'b')) (Lit 'c'))
        it "parse: \"ab*\" Just (Concat (Lit 'a') (Star (Lit 'b')))" $ 
            parse "ab*" `shouldBe` Just (Concat (Lit 'a') (Star (Lit 'b')))
        it "parse: \"(ab)*\" Just (Star (Concat (Lit 'a') (Lit 'b')))" $ 
            parse "(ab)*" `shouldBe` Just (Star (Concat (Lit 'a') (Lit 'b')))
        it "parse: \"a**\" Just (Star (Star (Lit 'a')))" $ 
            parse "a**" `shouldBe` Just (Star (Star (Lit 'a')))
        it "parse: \"a+\" Just (Concat (Lit 'a') (Star (Lit 'a')))" $ 
            parse "a+" `shouldBe` Just (Concat (Lit 'a') (Star (Lit 'a')))
        it "parse: \"a?\" Just (Union (Lit 'a') Empty)" $ 
            parse "a?" `shouldBe` Just (Union (Lit 'a') Empty)
        it "parse: \"\\*\" Just (Lit '*')" $ 
            parse "\\*" `shouldBe` Just (Lit '*')
        it "parse: \"(\" Nothing" $ 
            parse "(" `shouldBe` Nothing
        it "parse: \"a)\" Nothing" $ 
            parse "a)" `shouldBe` Nothing
        it "parse: \"*a\" Nothing" $ 
            parse "*a" `shouldBe` Nothing
        it "parse: \"a|\" Nothing" $ 
            parse "a|" `shouldBe` Just (Union (Lit 'a') Empty)

    describe "Matcher tests:" $ do
        it "match: Empty \"\"" $ 
            matches (Empty) "" `shouldBe` True
        it "no match: Empty \"a\"" $
            matches (Empty) "a" `shouldBe` False
        it "match: Lit 'a' \"a\"" $ 
            matches (Lit 'a') "a" `shouldBe` True
        it "no match: Lit 'a' \"b\"" $ 
            matches (Lit 'a') "b" `shouldBe` False
        it "no match: Lit 'a \"\"" $ 
            matches (Lit 'a') "" `shouldBe` False
        it "match: Concat (Lit 'a') (Lit 'b') \"ab\"" $ 
            matches (Concat (Lit 'a') (Lit 'b')) "ab" `shouldBe` True
        it "no match: Concat (Lit 'a') (Lit 'b') \"a\"" $ 
            matches (Concat (Lit 'a') (Lit 'b')) "a" `shouldBe` False
        it "match: Union (Lit 'a') (Lit 'b') \"a\"" $ 
            matches (Union (Lit 'a') (Lit 'b')) "a" `shouldBe` True
        it "match: Union (Lit 'a') (Lit 'b') \"b\"" $ 
            matches (Union (Lit 'a') (Lit 'b')) "b" `shouldBe` True
        it "no match: Union (Lit 'a') (Lit 'b') \"c\"" $ 
            matches (Union (Lit 'a') (Lit 'b')) "c" `shouldBe` False
        it "match: Star (Lit 'a') \"\"" $ 
            matches (Star (Lit 'a')) "" `shouldBe` True
        it "match: Star (Lit 'a') \"aaa\"" $ 
            matches (Star (Lit 'a')) "aaa" `shouldBe` True
        it "no match: Star (Lit 'a') \"aab\"" $ 
            matches (Star (Lit 'a')) "aab" `shouldBe` False
    
    describe "End-to-end: (a|b)*c" $ do
        it "match: ababc" $
            m "(a|b)*c" "ababc" `shouldBe` True
        it "match: c (no repetition of a or b)" $
            m "(a|b)*c" "c" `shouldBe` True
        it "match: abac" $
            m "(a|b)*c" "abac" `shouldBe` True
        it "no match: aba (no ending c)" $
            m "(a|b)*c" "aba" `shouldBe` False
        it "no match: \"\" " $
            m "(a|b)*c" "" `shouldBe` False
        it "no match: ababcc (two c)" $
            m "(a|b)*c" "ababcc" `shouldBe` False

    describe "End-to-end: (ab)*" $ do
        it "match: \"\"" $
            m "(ab)*" "" `shouldBe` True
        it "match: abab" $
            m "(ab)*" "abab" `shouldBe` True
        it "no match: aba" $
            m "(ab)*" "aba" `shouldBe` False

    describe "End-to-end: a?b" $ do
        it "match: b (a skipped)" $
            m "a?b" "b" `shouldBe` True
        it "match: ab" $
            m "a?b" "ab" `shouldBe` True
        it "no match: aab" $
            m "a?b" "aab" `shouldBe` False
        