module AstSpec where

import Test.Hspec
import Test.QuickCheck

import Ast
import Control.Monad
import Data.Either
import Data.List
import Data.Nat
import TestUtils
import Text.Parsec
import Types
import Utils

spec :: Spec
spec = do
    -- Ignore
    ignoreSpec

    -- Numbers
    parseNumVsUnparseNumSpec
    parseNumSpec
    unparseNumSpec

    -- Strings
    parseStrVsUnparseStrSpec
    parseStrSpec
    unparseStrSpec
    parseCharVsUnparseCharSpec
    parseCharSpec
    unparseCharSpec

    -- Identifiers
    parseIdentVsUnparseIdentSpec
    parseIdentSpec
    unparseIdentSpec
    parseIdentPartVsUnparseIdentPartSpec
    parseIdentPartSpec
    unparseIdentPartSpec

    -- Symbols
    parseSymbVsUnparseSymbSpec
    parseSymbSpec
    unparseSymbSpec

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

    -- Quoting
    quoteVsUnquoteSpec
    quoteSpec
    unquoteSpec

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
        let s =" \n\t\r\n\v# # 123 \" a ' [ ( ] ) { < } >\n" ++ " \n\t\r\n\v# # 123 \" a ' [ ( ] ) { < } >\r\n" ++ " \n\t\r\n\v# # 123 \" a ' [ ( ] ) { < } >"
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

-- Numbers
parseNumVsUnparseNumSpec :: Spec
parseNumVsUnparseNumSpec = describe "parseNum vs unparseNum" $ do
    it "composes parseNum and unparseNum into id into identity" $ do
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

-- Strings
parseStrVsUnparseStrSpec :: Spec
parseStrVsUnparseStrSpec = describe "parseStr vs unparseStr" $ do
    it "composes parseStr with unparseStr into id into id" $ do
        property $ \s -> do
            let unparsed = unparseStr s
            parse parseStr "tests" unparsed `shouldBe` Right s
            unparseStr <$> parse parseStr "tests" unparsed `shouldBe` Right unparsed

parseStrSpec :: Spec
parseStrSpec = describe "parseStr" $ do
    it "rejects empty string" $ do
        isLeft (parse parseStr "tests" "") `shouldBe` True

    it "rejects non-printable, non-backslash char in string" $ do
        forM_ [noEscapeChars ++ [c] | c <- "\b\f\n\t\b\0\31\127"] $ \s -> do
            isLeft (parse parseStr "tests" ("\"" ++ s ++ "\"")) `shouldBe` True

    it "rejects unsupported escape sequence" $ do
        forM_ [noEscapeChars ++ ['\\', c] | c <- "acdeghijklmopqsvwxyz"] $ \s -> do
            isLeft (parse parseStr "tests" ("\"" ++ s ++ "\"")) `shouldBe` True

    it "parses string without escaping (ascii)" $ do
        let s = noEscapeChars
        parse parseStr "tests" ("\"" ++ s ++ "\"") `shouldBe` Right s

    it "parses string without excaping (accents)" $ do
        let s = accentChars
        parse parseStr "tests" ("\"" ++ s ++ "\"") `shouldBe` Right s

    it "parses string with escaping (non-unicode)" $ do
        parse parseStr "tests" "\"\\\"\"" `shouldBe` Right "\""
        parse parseStr "tests" "\"\\\\\"" `shouldBe` Right "\\"
        parse parseStr "tests" "\"\\/\"" `shouldBe` Right "/"
        parse parseStr "tests" "\"\\b\"" `shouldBe` Right "\b"
        parse parseStr "tests" "\"\\f\"" `shouldBe` Right "\f"
        parse parseStr "tests" "\"\\n\"" `shouldBe` Right "\n"
        parse parseStr "tests" "\"\\r\"" `shouldBe` Right "\r"
        parse parseStr "tests" "\"\\t\"" `shouldBe` Right "\t"

    it "parses string with escaping (unicode)" $ do
        parse parseStr "tests" "\"\\u{0}\"" `shouldBe` Right "\0"
        parse parseStr "tests" "\"\\u{1}\"" `shouldBe` Right "\1"
        parse parseStr "tests" "\"\\u{2}\"" `shouldBe` Right "\2"
        parse parseStr "tests" "\"\\u{3}\"" `shouldBe` Right "\3"
        parse parseStr "tests" "\"\\u{4}\"" `shouldBe` Right "\4"
        parse parseStr "tests" "\"\\u{5}\"" `shouldBe` Right "\5"
        parse parseStr "tests" "\"\\u{6}\"" `shouldBe` Right "\6"
        parse parseStr "tests" "\"\\u{7}\"" `shouldBe` Right "\7"
        -- \b
        -- \t
        -- \n
        parse parseStr "tests" "\"\\u{b}\"" `shouldBe` Right "\11"
        -- \f
        -- \r
        parse parseStr "tests" "\"\\u{e}\"" `shouldBe` Right "\14"
        parse parseStr "tests" "\"\\u{f}\"" `shouldBe` Right "\15"
        parse parseStr "tests" "\"\\u{10}\"" `shouldBe` Right "\16"
        parse parseStr "tests" "\"\\u{11}\"" `shouldBe` Right "\17"
        parse parseStr "tests" "\"\\u{12}\"" `shouldBe` Right "\18"
        parse parseStr "tests" "\"\\u{13}\"" `shouldBe` Right "\19"
        parse parseStr "tests" "\"\\u{14}\"" `shouldBe` Right "\20"
        parse parseStr "tests" "\"\\u{15}\"" `shouldBe` Right "\21"
        parse parseStr "tests" "\"\\u{16}\"" `shouldBe` Right "\22"
        parse parseStr "tests" "\"\\u{17}\"" `shouldBe` Right "\23"
        parse parseStr "tests" "\"\\u{18}\"" `shouldBe` Right "\24"
        parse parseStr "tests" "\"\\u{19}\"" `shouldBe` Right "\25"
        parse parseStr "tests" "\"\\u{1a}\"" `shouldBe` Right "\26"
        parse parseStr "tests" "\"\\u{1b}\"" `shouldBe` Right "\27"
        parse parseStr "tests" "\"\\u{1c}\"" `shouldBe` Right "\28"
        parse parseStr "tests" "\"\\u{1d}\"" `shouldBe` Right "\29"
        parse parseStr "tests" "\"\\u{1e}\"" `shouldBe` Right "\30"
        parse parseStr "tests" "\"\\u{1f}\"" `shouldBe` Right "\31"
        parse parseStr "tests" "\"\\u{7f}\"" `shouldBe` Right "\127"

unparseStrSpec :: Spec
unparseStrSpec = describe "unparseStr" $ do
    it "unparses string without excaping (ascii)" $ do
        let s = noEscapeChars
        unparseStr s `shouldBe` ("\"" ++ s ++ "\"")

    it "unparses string without excaping (accents)" $ do
        let s = accentChars
        unparseStr s `shouldBe` ("\"" ++ s ++ "\"")

    it "unparses string with escaping (non-unicode)" $ do
        unparseStr "\"" `shouldBe` "\"\\\"\""
        unparseStr "\\" `shouldBe` "\"\\\\\""
        unparseStr "/" `shouldBe` "\"\\/\""
        unparseStr "\b" `shouldBe` "\"\\b\""
        unparseStr "\f" `shouldBe` "\"\\f\""
        unparseStr "\n" `shouldBe` "\"\\n\""
        unparseStr "\r" `shouldBe` "\"\\r\""
        unparseStr "\t" `shouldBe` "\"\\t\""

    it "unparses string with escaping (unicode)" $ do
        unparseStr "\0" `shouldBe` "\"\\u{0}\""
        unparseStr "\1" `shouldBe` "\"\\u{1}\""
        unparseStr "\2" `shouldBe` "\"\\u{2}\""
        unparseStr "\3" `shouldBe` "\"\\u{3}\""
        unparseStr "\4" `shouldBe` "\"\\u{4}\""
        unparseStr "\5" `shouldBe` "\"\\u{5}\""
        unparseStr "\6" `shouldBe` "\"\\u{6}\""
        unparseStr "\7" `shouldBe` "\"\\u{7}\""
        -- \b
        -- \t
        -- \n
        unparseStr "\11" `shouldBe` "\"\\u{b}\""
        -- \f
        -- \r
        unparseStr "\14" `shouldBe` "\"\\u{e}\""
        unparseStr "\15" `shouldBe` "\"\\u{f}\""
        unparseStr "\16" `shouldBe` "\"\\u{10}\""
        unparseStr "\17" `shouldBe` "\"\\u{11}\""
        unparseStr "\18" `shouldBe` "\"\\u{12}\""
        unparseStr "\19" `shouldBe` "\"\\u{13}\""
        unparseStr "\20" `shouldBe` "\"\\u{14}\""
        unparseStr "\21" `shouldBe` "\"\\u{15}\""
        unparseStr "\22" `shouldBe` "\"\\u{16}\""
        unparseStr "\23" `shouldBe` "\"\\u{17}\""
        unparseStr "\24" `shouldBe` "\"\\u{18}\""
        unparseStr "\25" `shouldBe` "\"\\u{19}\""
        unparseStr "\26" `shouldBe` "\"\\u{1a}\""
        unparseStr "\27" `shouldBe` "\"\\u{1b}\""
        unparseStr "\28" `shouldBe` "\"\\u{1c}\""
        unparseStr "\29" `shouldBe` "\"\\u{1d}\""
        unparseStr "\30" `shouldBe` "\"\\u{1e}\""
        unparseStr "\31" `shouldBe` "\"\\u{1f}\""
        unparseStr "\127" `shouldBe` "\"\\u{7f}\""

parseCharVsUnparseCharSpec :: Spec
parseCharVsUnparseCharSpec = describe "parseChar vs unparseChar" $ do
    it "composes parseChar with unparseChar into id into id" $ do
        property $ \c -> do
            let s = unparseChar c
            parse parseChar "tests" s `shouldBe` Right c
            unparseChar <$> parse parseChar "tests" s `shouldBe` Right s

parseCharSpec :: Spec
parseCharSpec = describe "parseChar" $ do
    it "rejects non-printable, non-backslash char" $ do
        forM_ [[c] | c <- "\b\f\n\t\b\0\31\127"] $ \s -> do
            isLeft (parse parseChar "tests" s) `shouldBe` True

    it "rejects char with unsupported escape sequence" $ do
        forM_ ['\\':[c] | c <- "acdeghijklmopqsvwxyz"] $ \s -> do
            isLeft (parse parseChar "tests" s) `shouldBe` True

    it "parses char without escaping (ascii)" $ do
        forM_ noEscapeChars $ \c -> do
            parse parseChar "tests" [c] `shouldBe` Right c

    it "parses char without excaping (accents)" $ do
        forM_ accentChars $ \c -> do
            parse parseChar "tests" [c] `shouldBe` Right c

    it "parses char with escaping (non-unicode)" $ do
        parse parseChar "tests" "\\\"" `shouldBe` Right '"'
        parse parseChar "tests" "\\\\" `shouldBe` Right '\\'
        parse parseChar "tests" "\\/" `shouldBe` Right '/'
        parse parseChar "tests" "\\b" `shouldBe` Right '\b'
        parse parseChar "tests" "\\f" `shouldBe` Right '\f'
        parse parseChar "tests" "\\n" `shouldBe` Right '\n'
        parse parseChar "tests" "\\r" `shouldBe` Right '\r'
        parse parseChar "tests" "\\t" `shouldBe` Right '\t'

    it "parses char with escaping (unicode)" $ do
        parse parseChar "tests" "\\u{0}" `shouldBe` Right '\0'
        parse parseChar "tests" "\\u{1}" `shouldBe` Right '\1'
        parse parseChar "tests" "\\u{2}" `shouldBe` Right '\2'
        parse parseChar "tests" "\\u{3}" `shouldBe` Right '\3'
        parse parseChar "tests" "\\u{4}" `shouldBe` Right '\4'
        parse parseChar "tests" "\\u{5}" `shouldBe` Right '\5'
        parse parseChar "tests" "\\u{6}" `shouldBe` Right '\6'
        parse parseChar "tests" "\\u{7}" `shouldBe` Right '\7'
        -- \b
        -- \t
        -- \n
        parse parseChar "tests" "\\u{b}" `shouldBe` Right '\11'
        -- \f
        -- \r
        parse parseChar "tests" "\\u{e}" `shouldBe` Right '\14'
        parse parseChar "tests" "\\u{f}" `shouldBe` Right '\15'
        parse parseChar "tests" "\\u{10}" `shouldBe` Right '\16'
        parse parseChar "tests" "\\u{11}" `shouldBe` Right '\17'
        parse parseChar "tests" "\\u{12}" `shouldBe` Right '\18'
        parse parseChar "tests" "\\u{13}" `shouldBe` Right '\19'
        parse parseChar "tests" "\\u{14}" `shouldBe` Right '\20'
        parse parseChar "tests" "\\u{15}" `shouldBe` Right '\21'
        parse parseChar "tests" "\\u{16}" `shouldBe` Right '\22'
        parse parseChar "tests" "\\u{17}" `shouldBe` Right '\23'
        parse parseChar "tests" "\\u{18}" `shouldBe` Right '\24'
        parse parseChar "tests" "\\u{19}" `shouldBe` Right '\25'
        parse parseChar "tests" "\\u{1a}" `shouldBe` Right '\26'
        parse parseChar "tests" "\\u{1b}" `shouldBe` Right '\27'
        parse parseChar "tests" "\\u{1c}" `shouldBe` Right '\28'
        parse parseChar "tests" "\\u{1d}" `shouldBe` Right '\29'
        parse parseChar "tests" "\\u{1e}" `shouldBe` Right '\30'
        parse parseChar "tests" "\\u{1f}" `shouldBe` Right '\31'
        parse parseChar "tests" "\\u{7f}" `shouldBe` Right '\127'

        parse parseChar "tests" "\\u{10FFFE}" `shouldBe` Right '\1114110'
        parse parseChar "tests" "\\u{10FFFF}" `shouldBe` Right '\1114111'

    it "rejects out-of-bounds" $ do
        isLeft (parse parseChar "tests" "\\u{110000}") `shouldBe` True
        isLeft (parse parseChar "tests" "\\u{110001}") `shouldBe` True

unparseCharSpec :: Spec
unparseCharSpec = describe "unparseChar" $ do
    it "unparses char without excaping (ascii)" $ do
        forM_ noEscapeChars $ \c -> do
            unparseChar c `shouldBe` [c]

    it "unparses char without excaping (accents)" $ do
        forM_ accentChars $ \c -> do
            unparseChar c `shouldBe` [c]

    it "unparses char with escaping (non-unicode)" $ do
        unparseChar '\"' `shouldBe` "\\\""
        unparseChar '\\' `shouldBe` "\\\\"
        unparseChar '/' `shouldBe` "\\/"
        unparseChar '\b' `shouldBe` "\\b"
        unparseChar '\f' `shouldBe` "\\f"
        unparseChar '\n' `shouldBe` "\\n"
        unparseChar '\r' `shouldBe` "\\r"
        unparseChar '\t' `shouldBe` "\\t"

    it "unparses char with escaping (unicode)" $ do
        unparseChar '\0' `shouldBe` "\\u{0}"
        unparseChar '\1' `shouldBe` "\\u{1}"
        unparseChar '\2' `shouldBe` "\\u{2}"
        unparseChar '\3' `shouldBe` "\\u{3}"
        unparseChar '\4' `shouldBe` "\\u{4}"
        unparseChar '\5' `shouldBe` "\\u{5}"
        unparseChar '\6' `shouldBe` "\\u{6}"
        unparseChar '\7' `shouldBe` "\\u{7}"
        -- \b
        -- \t
        -- \n
        unparseChar '\11' `shouldBe` "\\u{b}"
        -- \f
        -- \r
        unparseChar '\14' `shouldBe` "\\u{e}"
        unparseChar '\15' `shouldBe` "\\u{f}"
        unparseChar '\16' `shouldBe` "\\u{10}"
        unparseChar '\17' `shouldBe` "\\u{11}"
        unparseChar '\18' `shouldBe` "\\u{12}"
        unparseChar '\19' `shouldBe` "\\u{13}"
        unparseChar '\20' `shouldBe` "\\u{14}"
        unparseChar '\21' `shouldBe` "\\u{15}"
        unparseChar '\22' `shouldBe` "\\u{16}"
        unparseChar '\23' `shouldBe` "\\u{17}"
        unparseChar '\24' `shouldBe` "\\u{18}"
        unparseChar '\25' `shouldBe` "\\u{19}"
        unparseChar '\26' `shouldBe` "\\u{1a}"
        unparseChar '\27' `shouldBe` "\\u{1b}"
        unparseChar '\28' `shouldBe` "\\u{1c}"
        unparseChar '\29' `shouldBe` "\\u{1d}"
        unparseChar '\30' `shouldBe` "\\u{1e}"
        unparseChar '\31' `shouldBe` "\\u{1f}"
        unparseChar '\127' `shouldBe` "\\u{7f}"

-- Identifiers
parseIdentVsUnparseIdentSpec :: Spec
parseIdentVsUnparseIdentSpec = describe "parseIdent vs unparseIdent" $ do
    it "composes parseIdent and unparseIdent into id into id" $ do
        property $ \ident -> do
            let s = unparseIdent ident
            parse parseIdent "tests" s `shouldBe` Right ident
            unparseIdent <$> parse parseIdent "tests" s `shouldBe` Right s

parseIdentSpec :: Spec
parseIdentSpec = describe "parseIdent" $ do
    it "rejects empty string" $ do
        isLeft (parse parseIdent "tests" "") `shouldBe` True

    it "parses one part" $ do
        property $ \(IdentPart p) -> do
            parse parseIdent "tests" p `shouldBe` Right (Ident [p])

    it "parses two parts" $ do
        property $ \(IdentPart p1) (IdentPart p2) -> do
            let s = p1 ++ "." ++ p2
            parse parseIdent "tests" s `shouldBe` Right (Ident [p1, p2])

    it "parses two parts" $ do
        property $ \(IdentPart p1) (IdentPart p2) (IdentPart p3) -> do
            let s = p1 ++ "." ++ p2 ++ "." ++ p3
            parse parseIdent "tests" s `shouldBe` Right (Ident [p1, p2, p3])

    it "parses n parts" $ do
        property $ \(Ident ps) -> do
            let s = intercalate "." ps
            parse parseIdent "tests" s `shouldBe` Right (Ident ps)

unparseIdentSpec :: Spec
unparseIdentSpec = describe "unparseIdent" $ do
    it "returns empty string for empty list" $ do
        unparseIdent (Ident []) `shouldBe` ""

    it "intercalates no dots for single part" $ do
        property $ \(IdentPart p) -> do
            unparseIdent (Ident [p]) `shouldBe` p

    it "intercalates dots between two parts" $ do
        property $ \(IdentPart p1) (IdentPart p2) -> do
            let s = p1 ++ "." ++ p2
            unparseIdent (Ident [p1, p2]) `shouldBe` s

    it "intercalates dots between three parts" $ do
        property $ \(IdentPart p1) (IdentPart p2) (IdentPart p3) -> do
            let s = p1 ++ "." ++ p2 ++ "." ++ p3
            unparseIdent (Ident [p1, p2, p3]) `shouldBe` s

    it "intercalates dots between n parts" $ do
        property $ \(Ident ps) -> do
            let s = intercalate "." ps
            unparseIdent (Ident ps) `shouldBe` s

parseIdentPartVsUnparseIdentPartSpec :: Spec
parseIdentPartVsUnparseIdentPartSpec = describe "parseIdentPart vs unparseIdentPart" $ do
    it "composes parseIdentPart and unparseIdentPart into id into id" $ do
        property $ \(Ident (p:_)) -> do
            let s = unparseIdentPart p
            parse parseIdentPart "tests" s `shouldBe` Right p
            unparseIdentPart <$> parse parseIdentPart "tests" s `shouldBe` Right s

parseIdentPartSpec :: Spec
parseIdentPartSpec = describe "parseIdentPart" $ do
    it "rejects invalid first" $ do
        forM_ invalidFirsts $ \f -> do
            let s = f : validNexts
            isLeft (parse parseIdentPart "tests" s) `shouldBe` True

    it "parses successfully" $ do
        forM_ validFirsts $ \f -> do
            let s = f : validNexts
            parse parseIdentPart "tests" s `shouldBe` Right s

unparseIdentPartSpec :: Spec
unparseIdentPartSpec = describe "unparseIdentPart" $ do
    it "returns the input ident part" $ do
        property $ \s -> do
            unparseIdentPart s `shouldBe` s

-- Symbols
parseSymbVsUnparseSymbSpec :: Spec
parseSymbVsUnparseSymbSpec = describe "parseSymb vs unparseSymb" $ do
    it "composes parseSymb and unparseSymb into id" $ do
        property $ \n ident -> do
            let s = "'" ++ unlen n '\'' ++ unparseIdent ident
            parse parseSymb "tests" s `shouldBe` Right (Symb n ident)
            unparseSymb <$> parse parseSymb "tests" s `shouldBe` Right s

parseSymbSpec :: Spec
parseSymbSpec = describe "parseSymb" $ do
    it "rejects empty string" $ do
        isLeft (parse parseSymb "tests" "") `shouldBe` True

    it "parses one quote followed by ident" $ do
        property $ \ident -> do
            let s = '\'' : unparseIdent ident
            parse parseSymb "tests" s `shouldBe` Right (Symb Z ident)

    it "parses n+1 quotes followed by ident" $ do
        property $ \n ident -> do
            let s = "'" ++ unlen n '\'' ++ unparseIdent ident
            parse parseSymb "tests" s `shouldBe` Right (Symb n ident)

unparseSymbSpec :: Spec
unparseSymbSpec = describe "unparseSymb" $ do
    it "unparses one quote followed by ident" $ do
        property $ \ident -> do
            unparseSymb (Symb Z ident) `shouldBe` "'" ++ unparseIdent ident
    
    it "unparses n+1 quotes followed by ident" $ do
        property $ \n ident -> do
            unparseSymb (Symb n ident) `shouldBe` "'" ++ unlen n '\'' ++ unparseIdent ident

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
                parseList' k ([start] ++ unparseElem e ++ [end]) `shouldBe` Right [e]

        it "parses two elems" $ do
            property $ \e1 e2 -> 
                parseList' k ([start] ++ unparseElem e1 ++ " " ++ unparseElem e2 ++ [end]) `shouldBe` Right [e1, e2]

        it "parses three elems" $ do
            property $ \e1 e2 e3 -> 
                parseList' k ([start] ++ unparseElem e1 ++ " " ++ unparseElem e2 ++ " " ++ unparseElem e3 ++ [end]) `shouldBe` Right [e1, e2, e3]

        it "parses n elems" $ do
            property $ \(Few es) -> 
                parseList' k ([start] ++ unwords (map unparseElem es) ++ [end]) `shouldBe` Right es

unparseListSpec :: Spec
unparseListSpec = describe "unparseList" $ do
    forM_ kinds $ \k -> do
        let (start, end) = (getListStart k, getListEnd k)

        it "unparses zero elems" $ do
            unparseList' k [] `shouldBe` [start] ++ [end]

        it "unparses one elem" $ do
            property $ \e -> do
                unparseList' k [e] `shouldBe` [start] ++ unparseElem e ++ [end]

        it "unparses two elems" $ do
            property $ \e1 e2 -> do
                unparseList' k [e1, e2] `shouldBe` [start] ++ unparseElem e1 ++ " " ++ unparseElem e2 ++ [end]

        it "unparses three elems" $ do
            property $ \e1 e2 e3  -> do
                unparseList' k [e1, e2, e3] `shouldBe` [start] ++ unparseElem e1 ++ " " ++ unparseElem e2 ++ " " ++ unparseElem e3 ++ [end]

        it "unparses n elems" $ do
            property $ \es  -> do
                unparseList' k es `shouldBe` [start] ++ unwords (map unparseElem es) ++ [end]

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
            parseMany' (unparseElem e ++ "$") `shouldBe` Right [e]

    it "parses two elems" $ do
        property $ \e1 e2 -> do
            parseMany' (unparseElem e1 ++ " " ++ unparseElem e2 ++ "$") `shouldBe` Right [e1, e2]

    it "parses three elems" $ do
        property $ \e1 e2 e3 -> do
            parseMany' (unparseElem e1 ++ " " ++ unparseElem e2 ++ " " ++ unparseElem e3 ++ "$") `shouldBe` Right [e1, e2, e3]

    it "parses n elems" $ do
        property $ \(Few es) -> do
            parseMany' (unwords (map unparseElem es) ++ "$") `shouldBe` Right es

unparseManySpec :: Spec
unparseManySpec = describe "unparseMany" $ do
    it "unparses empty list" $ do
        unparseMany' [] `shouldBe` ""

    it "unparses one elem" $ do
        property $ \e -> do
            unparseMany' [e] `shouldBe` unparseElem e

    it "unparses two elems" $ do
        property $ \e1 e2 -> do
            unparseMany' [e1, e2] `shouldBe` unparseElem e1 ++ " " ++ unparseElem e2

    it "unparses three elems" $ do
        property $ \e1 e2 e3 -> do
            unparseMany' [e1, e2, e3] `shouldBe` unparseElem e1 ++ " " ++ unparseElem e2 ++ " " ++ unparseElem e3

    it "unparses n elems" $ do
        property $ \(Few es) -> do
            unparseMany' es `shouldBe` unwords (map unparseElem es)

-- Expressions
parseExprVsUnparseExprSpec :: Spec
parseExprVsUnparseExprSpec = describe "parseExpr vs unparseExpr" $ do
    it "composes parseExpr and unparseExpr into id" $ do
        property $ \e -> do
            let s = unparseExpr e
            parse (spaces >> parseExpr ignore) "tests" s `shouldBe` Right e
            unparseExpr <$> parse (spaces >> parseExpr ignore) "tests" s `shouldBe` Right s

parseExprSpec :: Spec
parseExprSpec = describe "parseExpr" $ do
    it "parses num" $ do
        property $ \p n -> do
            parse (parseExpr ignore) "tests" (unparseNum n) `shouldBe` Right (WithPos p $ AstNum n)

    it "parses str" $ do
        property $ \p s -> do
            parse (parseExpr ignore) "tests" (unparseStr s) `shouldBe` Right (WithPos p $ AstStr s)

    it "parses ident" $ do
        property $ \p ident -> do
            parse (parseExpr ignore) "tests" (unparseIdent ident) `shouldBe` Right (WithPos p $ AstIdent ident)

    it "parses symb" $ do
        property $ \p n ident -> do
            parse (parseExpr ignore) "tests" (unparseSymb $ Symb n ident) `shouldBe` Right (WithPos p $ AstSymb $ Symb n ident)

    it "parses list" $ do
        property $ \p (Few es) -> do
            forM_ kinds $ \k -> do
                parse (parseExpr ignore) "tests" (unparseList k unparseExpr es) `shouldBe` Right (WithPos p $ AstList k es)

unparseExprSpec :: Spec
unparseExprSpec = describe "unparseExpr" $ do
    it "unparses num" $ do
        property $ \p n -> do
            unparseExpr (WithPos p $ AstNum n) `shouldBe` unparseNum n

    it "unparses str" $ do
        property $ \p s -> do
            unparseExpr (WithPos p $ AstStr s) `shouldBe` unparseStr s

    it "unparses ident" $ do
        property $ \p ident -> do
            unparseExpr (WithPos p $ AstIdent ident) `shouldBe` unparseIdent ident

    it "unparses symb" $ do
        property $ \p n ident -> do
            unparseExpr (WithPos p $ AstSymb $ Symb n ident) `shouldBe` unparseSymb (Symb n ident)

    it "unparses list" $ do
        property $ \p (Few es) -> do
            forM_ kinds $ \k -> do
                unparseExpr (WithPos p $ AstList k es) `shouldBe` unparseList k unparseExpr es

-- Quoting
quoteVsUnquoteSpec :: Spec
quoteVsUnquoteSpec = describe "quote vs unquote" $ do
    it "composes quote and unquote into id" $ do
        property $ \e1 (UnquoteValid e2) -> do
            quote <$> unquote (quote e1) `shouldBe` Right (quote e1)
            quote <$> unquote e2 `shouldBe` Right e2

quoteSpec :: Spec
quoteSpec = describe "quote" $ do
    it "converts numbers into themselves" $ do
        property $ \p n -> do
            let e = WithPos p $ AstNum n
            quote e `shouldBe` e

    it "converts strings into themselves" $ do
        property $ \p s -> do
            let e = WithPos p $ AstStr s
            quote e `shouldBe` e

    it "converts identifiers into single-quoted symbols" $ do
        property $ \p i -> do
            quote (WithPos p $ AstIdent i) `shouldBe` WithPos p (AstSymb $ Symb Z i)

    it "converts symbols into one-more-quoted symbols" $ do
        property $ \p n ident -> do
            quote (WithPos p $ AstSymb $ Symb n ident) `shouldBe` WithPos p (AstSymb $ Symb (S n) ident)

    it "converts lists into 'list-prefixed lists" $ do
        property $ \p (Few es) -> do
            quote (WithPos p $ AstList KindList es) `shouldBe`
                WithPos p (AstList KindList $ map quote $ toForm p KindList es)

    it "converts dicts into 'dict-prefixed lists" $ do
        property $ \p (Few es) -> do
            quote (WithPos p $ AstList KindDict es) `shouldBe`
                WithPos p (AstList KindList $ map quote $ toForm p KindDict es)

    it "converts forms into lists" $ do
        property $ \p (Few es) -> do
            quote (WithPos p $ AstList KindForm es) `shouldBe`
                WithPos p (AstList KindList $ map quote es)

unquoteSpec :: Spec
unquoteSpec = describe "unquote" $ do
    it "converts numbers into themselves" $ do
        property $ \p n -> do
            let e = WithPos p $ AstNum n
            unquote e `shouldBe` Right e

    it "converts strings into themselves" $ do
        property $ \p s -> do
            let e = WithPos p $ AstStr s
            unquote e `shouldBe` Right e

    it "rejects identifiers" $ do
        property $ \p i -> do
            let e = WithPos p $ AstIdent i
            unquote e `shouldBe` Left ("Unquote: unexpected identifier: " ++ unparseIdent i)

    it "converts single-quoted symbols into identifiers" $ do
        property $ \p i -> do
            unquote (WithPos p $ AstSymb $ Symb Z i) `shouldBe` Right (WithPos p $ AstIdent i)

    it "converts two-or-more-quoted symbols into one-less-quoted symbol" $ do
        property $ \p n i -> do
            unquote (WithPos p $ AstSymb $ Symb (S n) i) `shouldBe` Right (WithPos p $ AstSymb $ Symb n i)

    it "converts lists into forms" $ do
        property $ \p (UnquoteValids es) -> do
            let list = AstList KindList es
                mactual = unquote $ WithPos p list
            isRight mactual `shouldBe` True
            mactual `shouldBe` (WithPos p . AstList KindForm <$> mapM unquote es)
    
    it "rejects dictionaries" $ do
        property $ \p (Few es) -> do
            let dictionary = AstList KindDict es
            unquote (WithPos p dictionary) `shouldBe`
                Left ("Unquote: unexpected dictionary: " ++ unparseList KindDict unparseExpr es)

    it "rejects forms" $ do
        property $ \p (Few es) -> do
            let form = AstList KindForm es
            unquote (WithPos p form) `shouldBe`
                Left ("Unquote: unexpected form: " ++ unparseList KindForm unparseExpr es)

parseList' k = parse (parseList k ignore parseElem) "tests"
unparseList' k = unparseList k unparseElem

parseMany' = parse (parseMany spaces parseElem $ void $ char '$') "tests"
unparseMany' es = unparseMany unparseElem es