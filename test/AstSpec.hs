module AstSpec where

import Test.Hspec
import Test.QuickCheck

import Ast
import Control.Monad
import Data.Char
import Data.Either
import Data.Ident
import Data.List
import Data.Nat
import Data.Symb
import Data.WithPos
import Numeric
import TestUtils
import Text.Parsec
import Types

spec :: Spec
spec = do
    -- Ignore & comment
    ignoreSpec
    commentSpec

    -- Numbers
    parseNumVsUnparseNumSpec
    parseNumSpec
    unparseNumSpec

    -- Lists
    parseListVsUnparseListSpec
    parseListSpec
    unparseListSpec
    getListStartSpec
    getListEndSpec
    parseManyVsUnparseManySpec
    parseManySpec
    unparseManySpec

    -- Expressions
    parseExprVsUnparseExprSpec
    parseExprSpec
    unparseExprSpec

-- AST
ignoreSpec :: Spec
ignoreSpec = describe "ignore" $ do
    it "parses empty string" $ do
        parse ignore "tests" "" `shouldBe` Right ()

    it "parses unprintable string" $ do
        let s = "\0\1\2\3\4\5\6\7\8\9\10\11\12\13\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31\32\127"
        parse ignore "tests" s `shouldBe` Right ()

    it "parses comment hash (lf)" $ do
        let s = "#\n"
        parse ignore "tests" s `shouldBe` Right ()

    it "parses comment hash (crlf)" $ do
        let s = "#\r\n"
        parse ignore "tests" s `shouldBe` Right ()

    it "parses comment hash (eof)" $ do
        let s = "#"
        parse ignore "tests" s `shouldBe` Right ()

    it "parses single comment (lf)" $ do
        let s = "# # 123 \" a ' [ ( ] ) { < } > \n"
        parse ignore "tests" s `shouldBe` Right ()

    it "parses single comment (crlf)" $ do
        let s = "# # 123 \" a ' [ ( ] ) { < } > \r\n"
        parse ignore "tests" s `shouldBe` Right ()

    it "parses single comment (eof)" $ do
        let s = "# # 123 \" a ' [ ( ] ) { < } >"
        parse ignore "tests" s `shouldBe` Right ()

    it "parses whitespace and comments" $ do
        let s =     " \n\t\r\n\v# # 123 \" a ' [ ( ] ) { < } >\n"
                ++  " \n\t\r\n\v# # 123 \" a ' [ ( ] ) { < } >\r\n"
                ++  " \n\t\r\n\v# # 123 \" a ' [ ( ] ) { < } >"
        parse ignore "tests" s `shouldBe` Right ()

    it "stops at/rejects non-whitespace, non-comment" $ do
        parse ignore "tests" "123" `shouldBe` Right ()
        isLeft (parse (ignore >> eof) "tests" "123") `shouldBe` True

        parse ignore "tests" "\"" `shouldBe` Right ()
        isLeft (parse (ignore >> eof) "tests" "\"") `shouldBe` True

        parse ignore "tests" "a" `shouldBe` Right ()
        isLeft (parse (ignore >> eof) "tests" "a") `shouldBe` True

        parse ignore "tests" "'" `shouldBe` Right ()
        isLeft (parse (ignore >> eof) "tests" "'") `shouldBe` True

        parse ignore "tests" "[" `shouldBe` Right ()
        isLeft (parse (ignore >> eof) "tests" "[") `shouldBe` True

        parse ignore "tests" "{" `shouldBe` Right ()
        isLeft (parse (ignore >> eof) "tests" "{") `shouldBe` True

        parse ignore "tests" "(" `shouldBe` Right ()
        isLeft (parse (ignore >> eof) "tests" "(") `shouldBe` True

commentSpec :: Spec
commentSpec = describe "comment" $ do
    it "rejects empty string" $ do
        isLeft (parse comment "tests" "") `shouldBe` True

    it "rejects unprintable string" $ do
        let s = "\0\1\2\3\4\5\6\7\8\9\10\11\12\13\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31\32\127"
        isLeft (parse comment "tests" s) `shouldBe` True

    it "rejects whitespace" $ do
        let s = " \n\t\r\n\v"
        isLeft (parse comment "tests" s) `shouldBe` True

    it "rejects non-comment" $ do
        isLeft (parse comment "tests" "123") `shouldBe` True
        isLeft (parse comment "tests" "\"") `shouldBe` True
        isLeft (parse comment "tests" "a") `shouldBe` True
        isLeft (parse comment "tests" "'") `shouldBe` True
        isLeft (parse comment "tests" "[") `shouldBe` True
        isLeft (parse comment "tests" "{") `shouldBe` True
        isLeft (parse comment "tests" "(") `shouldBe` True

    it "parses comment hash (lf)" $ do
        let s = "#\n"
        parse comment "tests" s `shouldBe` Right ()

    it "parses comment hash (crlf)" $ do
        let s = "#\r\n"
        parse comment "tests" s `shouldBe` Right ()

    it "parses comment hash (eof)" $ do
        let s = "#"
        parse comment "tests" s `shouldBe` Right ()

    it "parses single comment (lf)" $ do
        let s = "# # 123 \" a ' [ ( ] ) { < } > \n"
        parse comment "tests" s `shouldBe` Right ()

    it "parses single comment (crlf)" $ do
        let s = "# # 123 \" a ' [ ( ] ) { < } > \r\n"
        parse comment "tests" s `shouldBe` Right ()

    it "parses single comment (eof)" $ do
        let s = "# # 123 \" a ' [ ( ] ) { < } >"
        parse comment "tests" s `shouldBe` Right ()

-- Numbers
parseNumVsUnparseNumSpec :: Spec
parseNumVsUnparseNumSpec = describe "parseNum vs unparseNum" $ do
    it "composes parseNum and unparseNum into identity" $ do
        property $ \n -> do
            let s = unparseNum n
            parse parseNum "tests" s `shouldBe` Right n
            unparseNum <$> parse parseNum "tests" s `shouldBe` Right s

parseNumSpec :: Spec
parseNumSpec = describe "parseNum" $ do
    it "rejects empty string" $ do
        isLeft (parse parseNum "tests" "") `shouldBe` True

    it "parses integers" $ do
        property $ \n -> do
            parse parseNum "tests" (show (n :: Int)) `shouldBe` Right (fromIntegral n)

    it "parses doubles with decimal part" $ do
        property $ \intPart (Positive decPart) -> do
            let _ = (intPart :: Integer, decPart :: Integer)
                d = read $ show intPart ++ "." ++ show decPart
            parse parseNum "tests" (show d) `shouldBe` Right d

    it "parses doubles with exponential part" $ do
        property $ \intPart (Positive decPart) expPart -> do
            let _ = (intPart :: Integer, decPart :: Integer, expPart :: Integer)
                s1 = show intPart ++ "." ++ show (decPart + 1) ++ "e" ++ show expPart
                s2 = show intPart ++ "." ++ show (decPart + 1) ++ "E" ++ show expPart
                d1 = read s1
                d2 = read s2
            parse parseNum "tests" s1 `shouldBe` Right d1
            parse parseNum "tests" s1 `shouldBe` Right d2

    it "parses doubles" $ do
        property $ \d -> do
            parse parseNum "tests" (show d) `shouldBe` Right d

unparseNumSpec :: Spec
unparseNumSpec = describe "unparseNum" $ do
    it "returns show<int> for any integer" $ do
        property $ \n -> do
            unparseNum (fromIntegral n) `shouldBe` show (n :: Int)

    it "returns show<double> for any non-integer" $ do
        property $ \d -> do
            unparseNum (d + 0.1) `shouldBe` show (d + 0.1)

-- Lists
parseListVsUnparseListSpec :: Spec
parseListVsUnparseListSpec = describe "parseList vs unparseList" $ do
    forM_ kinds $ \k -> do
        it "composes parseList and unparseList into id" $ do
            property $ \(Few es) -> do
                let s = unparseList' k es
                parseList' k s `shouldBe` Right es
                unparseList' k <$> parseList' k s `shouldBe` Right s
           
parseListSpec :: Spec
parseListSpec = describe "parseList" $ do
    forM_ kinds $ \k -> do
        let (start, end) = (getListStart k, getListEnd k)

        it "rejects an empty string" $ do
            isLeft (parseList' k "") `shouldBe` True

        it "parses no elems" $ do
            parseList' k ([start] ++ [end]) `shouldBe` Right []

        it "parses one elem" $ do
            property $ \e ->
                parseList' k ([start] ++ unparseElem' e ++ [end]) `shouldBe` Right [e]

        it "parses two elems" $ do
            property $ \e1 e2 ->
                parseList' k ([start] ++ unparseElem' e1 ++ unparseElem' e2 ++ [end]) `shouldBe` Right [e1, e2]

        it "parses three elems" $ do
            property $ \e1 e2 e3 ->
                parseList' k ([start] ++ unparseElem' e1 ++ unparseElem' e2 ++ unparseElem' e3 ++ [end]) `shouldBe` Right [e1, e2, e3]

        it "parses n elems" $ do
            property $ \(Few es) ->
                parseList' k ([start] ++ concatMap unparseElem' es ++ [end]) `shouldBe` Right es

unparseListSpec :: Spec
unparseListSpec = describe "unparseList" $ do
    forM_ kinds $ \k -> do
        let (start, end) = (getListStart k, getListEnd k)

        it "unparses zero elems" $ do
            unparseList' k [] `shouldBe` [start] ++ [end]

        it "unparses one elem" $ do
            property $ \e -> do
                unparseList' k [e] `shouldBe` [start] ++ unparseElem' e ++ [end]

        it "unparses two elems" $ do
            property $ \e1 e2 -> do
                unparseList' k [e1, e2] `shouldBe` [start] ++ unparseElem' e1 ++ unparseElem' e2 ++ [end]

        it "unparses three elems" $ do
            property $ \e1 e2 e3  -> do
                unparseList' k [e1, e2, e3] `shouldBe` [start] ++ unparseElem' e1 ++ unparseElem' e2 ++ unparseElem' e3 ++ [end]

        it "unparses n elems" $ do
            property $ \es  -> do
                unparseList' k es `shouldBe` [start] ++ concatMap unparseElem' es ++ [end]

getListStartSpec :: Spec
getListStartSpec = describe "getListStart" $ do
    it "returns start for kind" $ do
        getListStart KindList `shouldBe` '['
        getListStart KindDict `shouldBe` '{'
        getListStart KindForm `shouldBe` '('

getListEndSpec :: Spec
getListEndSpec = describe "getListEnd" $ do
    it "returns end for kind" $ do
        getListEnd KindList `shouldBe` ']'
        getListEnd KindDict `shouldBe` '}'
        getListEnd KindForm `shouldBe` ')'

parseManyVsUnparseManySpec :: Spec
parseManyVsUnparseManySpec = describe "parseMany vs unparseMany" $ do
    it "composes parseMany and unparseMany into id" $ do
        property $ \(Few es) -> do
            parseMany' (unparseMany' es ++ "$") `shouldBe` Right es

parseManySpec :: Spec
parseManySpec = describe "parseMany" $ do
    it "rejects empty string" $ do
        isLeft (parseMany' "") `shouldBe` True

    it "parses no elems" $ do
        parseMany' ("$") `shouldBe` Right []

    it "parses no elems with whitespace" $ do
        parseMany' "   $" `shouldBe` Right []

    it "parses one elem" $ do
        property $ \e -> do
            parseMany' (unparseElem' e ++ "$") `shouldBe` Right [e]

    it "parses two elems" $ do
        property $ \e1 e2 -> do
            parseMany' (unparseElem' e1 ++ unparseElem' e2 ++ "$") `shouldBe` Right [e1, e2]

    it "parses three elems" $ do
        property $ \e1 e2 e3 -> do
            parseMany' (unparseElem' e1 ++ unparseElem' e2 ++ unparseElem' e3 ++ "$") `shouldBe` Right [e1, e2, e3]

    it "parses n elems" $ do
        property $ \(Few es) -> do
            parseMany' (concatMap unparseElem' es ++ "$") `shouldBe` Right es

unparseManySpec :: Spec
unparseManySpec = describe "unparseMany" $ do
    it "unparses empty list" $ do
        unparseMany' [] `shouldBe` ""
        unparseMany' [] `shouldBe` unparseElem Nothing

    it "unparses one elem" $ do
        property $ \e -> do
            unparseMany' [e] `shouldBe` unparseElem' e

    it "unparses two elems" $ do
        property $ \e1 e2 -> do
            unparseMany' [e1, e2] `shouldBe` unparseElem' e1 ++ unparseElem' e2

    it "unparses three elems" $ do
        property $ \e1 e2 e3 -> do
            unparseMany' [e1, e2, e3] `shouldBe` unparseElem' e1 ++ unparseElem' e2 ++ unparseElem' e3

    it "unparses n elems" $ do
        property $ \(Few es) -> do
            unparseMany' es `shouldBe` concatMap unparseElem' es

-- Expressions
parseExprVsUnparseExprSpec :: Spec
parseExprVsUnparseExprSpec = describe "parseExpr vs unparseExpr" $ do
    it "composes parseExpr and unparseExpr into id" $ do
        let f Nothing = ""; f (Just e) = unparseExpr f e ++ " "
            g = parseExpr ignore g
        property $ \e -> do
            let s = unparseExpr f e
            parse g "tests" s `shouldBe` Right e
            unparseExpr f <$> parse g "tests" s `shouldBe` Right s

parseExprSpec :: Spec
parseExprSpec = describe "parseExpr" $ do
    it "parses num" $ do
        property $ \p n -> do
            parse (parseExpr ignore undefined) "tests" (unparseNum n) `shouldBe` Right (WithPos p $ AstNum n)

    it "parses str" $ do
        property $ \p s -> do
            parse (parseExpr ignore undefined) "tests" (unparseStr s) `shouldBe` Right (WithPos p $ AstStr s)

    it "parses ident" $ do
        property $ \p ident -> do
            parse (parseExpr ignore undefined) "tests" (unparseIdent ident) `shouldBe` Right (WithPos p $ AstIdent ident)

    it "parses symb" $ do
        property $ \p n ident -> do
            parse (parseExpr ignore undefined) "tests" (unparseSymb $ Symb n ident) `shouldBe` Right (WithPos p $ AstSymb $ Symb n ident)

    it "parses list" $ do
        let f Nothing = ""; f (Just e) = unparseExpr f e ++ " "
            g = parseExpr ignore g
        property $ \p (Few es) -> do
            forM_ kinds $ \k -> do
                parse g "tests" (unparseList k f es) `shouldBe` Right (WithPos p $ AstList k es)

unparseExprSpec :: Spec
unparseExprSpec = describe "unparseExpr" $ do
    it "unparses num" $ do
        property $ \p n -> do
            unparseExpr undefined (WithPos p $ AstNum n) `shouldBe` unparseNum n

    it "unparses str" $ do
        property $ \p s -> do
            unparseExpr undefined (WithPos p $ AstStr s) `shouldBe` unparseStr s

    it "unparses ident" $ do
        property $ \p ident -> do
            unparseExpr undefined (WithPos p $ AstIdent ident) `shouldBe` unparseIdent ident

    it "unparses symb" $ do
        property $ \p n ident -> do
            unparseExpr undefined (WithPos p $ AstSymb $ Symb n ident) `shouldBe` unparseSymb (Symb n ident)

    it "unparses list" $ do
        let f Nothing = ""; f (Just e) = unparseExpr f e
        property $ \p (Few es) -> do
            forM_ kinds $ \k -> do
                unparseExpr f (WithPos p $ AstList k es) `shouldBe` unparseList k f es

unparseElem' = unparseElem.Just

parseList' k = parse (parseList k ignore parseElem) "tests"
unparseList' k = unparseList k unparseElem

parseMany' = parse (parseMany spaces parseElem $ void $ char '$') "tests"
unparseMany' es = unparseMany unparseElem es