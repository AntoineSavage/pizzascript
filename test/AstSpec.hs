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

    -- Strings
    parseStrVsUnparseStrSpec
    parseStrSpec
    unparseStrSpec
    parseCharVsUnparseCharSpec
    parseCharSpec
    unparseCharSpec
    parseHexCodepointSpec

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

-- Strings
parseStrVsUnparseStrSpec :: Spec
parseStrVsUnparseStrSpec = describe "parseStr vs unparseStr" $ do
    it "composes parseStr with unparseStr into id" $ do
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
        parse parseStr "tests" "\"\\u{8}\"" `shouldBe` Right "\8"
        parse parseStr "tests" "\"\\u{9}\"" `shouldBe` Right "\9"
        parse parseStr "tests" "\"\\u{a}\"" `shouldBe` Right "\10"
        parse parseStr "tests" "\"\\u{b}\"" `shouldBe` Right "\11"
        parse parseStr "tests" "\"\\u{c}\"" `shouldBe` Right "\12"
        parse parseStr "tests" "\"\\u{d}\"" `shouldBe` Right "\13"
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
        unparseStr "\8" `shouldBe` "\"\\b\""
        unparseStr "\9" `shouldBe` "\"\\t\""
        unparseStr "\10" `shouldBe` "\"\\n\""
        unparseStr "\11" `shouldBe` "\"\\u{b}\""
        unparseStr "\12" `shouldBe` "\"\\f\""
        unparseStr "\13" `shouldBe` "\"\\r\""
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
    it "composes parseChar with unparseChar into id" $ do
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
        parse parseChar "tests" "\\u{8}" `shouldBe` Right '\8'
        parse parseChar "tests" "\\u{9}" `shouldBe` Right '\9'
        parse parseChar "tests" "\\u{a}" `shouldBe` Right '\10'
        parse parseChar "tests" "\\u{b}" `shouldBe` Right '\11'
        parse parseChar "tests" "\\u{c}" `shouldBe` Right '\12'
        parse parseChar "tests" "\\u{d}" `shouldBe` Right '\13'
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
        unparseChar '\8' `shouldBe` "\\b"
        unparseChar '\9' `shouldBe` "\\t"
        unparseChar '\10' `shouldBe` "\\n"
        unparseChar '\11' `shouldBe` "\\u{b}"
        unparseChar '\12' `shouldBe` "\\f"
        unparseChar '\13' `shouldBe` "\\r"
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

parseHexCodepointSpec :: Spec
parseHexCodepointSpec = describe "parseHexCodepoint" $ do
    forM_ [id, map toUpper] $ \f -> do
        it "parses hex codepoint" $ do
            parse (parseHexCodepoint $ return $ f "0") "tests" "" `shouldBe` Right (f "0")
            parse (parseHexCodepoint $ return $ f "1") "tests" "" `shouldBe` Right (f "1")
            parse (parseHexCodepoint $ return $ f "2") "tests" "" `shouldBe` Right (f "2")
            parse (parseHexCodepoint $ return $ f "3") "tests" "" `shouldBe` Right (f "3")
            parse (parseHexCodepoint $ return $ f "4") "tests" "" `shouldBe` Right (f "4")
            parse (parseHexCodepoint $ return $ f "5") "tests" "" `shouldBe` Right (f "5")
            parse (parseHexCodepoint $ return $ f "6") "tests" "" `shouldBe` Right (f "6")
            parse (parseHexCodepoint $ return $ f "7") "tests" "" `shouldBe` Right (f "7")
            parse (parseHexCodepoint $ return $ f "8") "tests" "" `shouldBe` Right (f "8")
            parse (parseHexCodepoint $ return $ f "9") "tests" "" `shouldBe` Right (f "9")
            parse (parseHexCodepoint $ return $ f "a") "tests" "" `shouldBe` Right (f "a")
            parse (parseHexCodepoint $ return $ f "b") "tests" "" `shouldBe` Right (f "b")
            parse (parseHexCodepoint $ return $ f "c") "tests" "" `shouldBe` Right (f "c")
            parse (parseHexCodepoint $ return $ f "d") "tests" "" `shouldBe` Right (f "d")
            parse (parseHexCodepoint $ return $ f "e") "tests" "" `shouldBe` Right (f "e")
            parse (parseHexCodepoint $ return $ f "f") "tests" "" `shouldBe` Right (f "f")
            parse (parseHexCodepoint $ return $ f "10") "tests" "" `shouldBe` Right (f "10")
            parse (parseHexCodepoint $ return $ f "11") "tests" "" `shouldBe` Right (f "11")
            parse (parseHexCodepoint $ return $ f "12") "tests" "" `shouldBe` Right (f "12")
            parse (parseHexCodepoint $ return $ f "13") "tests" "" `shouldBe` Right (f "13")
            parse (parseHexCodepoint $ return $ f "14") "tests" "" `shouldBe` Right (f "14")
            parse (parseHexCodepoint $ return $ f "15") "tests" "" `shouldBe` Right (f "15")
            parse (parseHexCodepoint $ return $ f "16") "tests" "" `shouldBe` Right (f "16")
            parse (parseHexCodepoint $ return $ f "17") "tests" "" `shouldBe` Right (f "17")
            parse (parseHexCodepoint $ return $ f "18") "tests" "" `shouldBe` Right (f "18")
            parse (parseHexCodepoint $ return $ f "19") "tests" "" `shouldBe` Right (f "19")
            parse (parseHexCodepoint $ return $ f "1a") "tests" "" `shouldBe` Right (f "1a")
            parse (parseHexCodepoint $ return $ f "1b") "tests" "" `shouldBe` Right (f "1b")
            parse (parseHexCodepoint $ return $ f "1c") "tests" "" `shouldBe` Right (f "1c")
            parse (parseHexCodepoint $ return $ f "1d") "tests" "" `shouldBe` Right (f "1d")
            parse (parseHexCodepoint $ return $ f "1e") "tests" "" `shouldBe` Right (f "1e")
            parse (parseHexCodepoint $ return $ f "1f") "tests" "" `shouldBe` Right (f "1f")
            parse (parseHexCodepoint $ return $ f "7f") "tests" "" `shouldBe` Right (f "7f")

            parse (parseHexCodepoint $ return $ f "10fffe") "tests" "" `shouldBe` Right (f "10fffe")
            parse (parseHexCodepoint $ return $ f "10ffff") "tests" "" `shouldBe` Right (f "10ffff")

        it "parses hex codepoint (prop)" $ do
            property $ \(ValidCodepoint i) -> do
                let s = showHex i ""
                parse (parseHexCodepoint $ return $ f s) "tests" "" `shouldBe` Right (f s)

        it "rejects out-of-bounds" $ do
            isLeft (parse (parseHexCodepoint $ return "110000") "tests" "") `shouldBe` True
            isLeft (parse (parseHexCodepoint $ return "110001") "tests" "") `shouldBe` True

        it "rejects out-of-bounds (prop)" $ do
            property $ \(InvalidCodepoint i) -> do
                let s = showHex i ""
                isLeft (parse (parseHexCodepoint $ return $ f s) "tests" "") `shouldBe` True

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