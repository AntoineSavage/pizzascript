module Ops.StrSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Char
import Data.Either
import Numeric
import Ops.Str
import TestUtils
import Text.Parsec
import Types.Str

spec :: Spec
spec = do
    parseStrVsUnparseStrSpec
    parseStrSpec
    unparseStrSpec
    parseCharVsUnparseCharSpec
    parseCharSpec
    unparseCharSpec
    parseHexCodepointSpec

parseStrVsUnparseStrSpec :: Spec
parseStrVsUnparseStrSpec = describe "parseStr' vs unparseStr" $ do
    it "composes parseStr' with unparseStr' into id" $ do
        property $ \s -> do
            let unparsed = unparseStr' s
            parse parseStr' "tests" unparsed `shouldBe` Right s
            unparseStr' <$> parse parseStr' "tests" unparsed `shouldBe` Right unparsed

parseStrSpec :: Spec
parseStrSpec = describe "parseStr" $ do
    it "rejects empty string" $ do
        leftAsStr (parse parseStr' "tests" "") `shouldContain` "unexpected end of input"

    it "rejects non-printable, non-backslash char in string" $ do
        forM_ [noEscapeChars ++ [c] | c <- "\b\f\n\t\b\0\31\127"] $ \s -> do
            leftAsStr (parse parseStr' "tests" ("\"" ++ s ++ "\"")) `shouldContain`
                ("Unprintable characters must be escaped: " ++ let c:_ = reverse s in unparseChar c)

    it "rejects unsupported escape sequence" $ do
        forM_ [noEscapeChars ++ ['\\', c] | c <- "acdeghijklmopqsvwxyz"] $ \s -> do
            leftAsStr (parse parseStr' "tests" ("\"" ++ s ++ "\"")) `shouldContain`
                ("Unsupported escape sequence: \\" ++ let c:_ = reverse s in unparseChar c)

    it "parses string without escaping (ascii)" $ do
        let s = noEscapeChars
        parse parseStr' "tests" ("\"" ++ s ++ "\"") `shouldBe` Right s

    it "parses string without excaping (accents)" $ do
        let s = accentChars
        parse parseStr' "tests" ("\"" ++ s ++ "\"") `shouldBe` Right s

    it "parses string with escaping (non-unicode)" $ do
        parse parseStr' "tests" "\"\\\"\"" `shouldBe` Right "\""
        parse parseStr' "tests" "\"\\\\\"" `shouldBe` Right "\\"
        parse parseStr' "tests" "\"\\/\"" `shouldBe` Right "/"
        parse parseStr' "tests" "\"\\b\"" `shouldBe` Right "\b"
        parse parseStr' "tests" "\"\\f\"" `shouldBe` Right "\f"
        parse parseStr' "tests" "\"\\n\"" `shouldBe` Right "\n"
        parse parseStr' "tests" "\"\\r\"" `shouldBe` Right "\r"
        parse parseStr' "tests" "\"\\t\"" `shouldBe` Right "\t"

    it "parses string with escaping (unicode)" $ do
        parse parseStr' "tests" "\"\\u{0}\"" `shouldBe` Right "\0"
        parse parseStr' "tests" "\"\\u{1}\"" `shouldBe` Right "\1"
        parse parseStr' "tests" "\"\\u{2}\"" `shouldBe` Right "\2"
        parse parseStr' "tests" "\"\\u{3}\"" `shouldBe` Right "\3"
        parse parseStr' "tests" "\"\\u{4}\"" `shouldBe` Right "\4"
        parse parseStr' "tests" "\"\\u{5}\"" `shouldBe` Right "\5"
        parse parseStr' "tests" "\"\\u{6}\"" `shouldBe` Right "\6"
        parse parseStr' "tests" "\"\\u{7}\"" `shouldBe` Right "\7"
        parse parseStr' "tests" "\"\\u{8}\"" `shouldBe` Right "\8"
        parse parseStr' "tests" "\"\\u{9}\"" `shouldBe` Right "\9"
        parse parseStr' "tests" "\"\\u{a}\"" `shouldBe` Right "\10"
        parse parseStr' "tests" "\"\\u{b}\"" `shouldBe` Right "\11"
        parse parseStr' "tests" "\"\\u{c}\"" `shouldBe` Right "\12"
        parse parseStr' "tests" "\"\\u{d}\"" `shouldBe` Right "\13"
        parse parseStr' "tests" "\"\\u{e}\"" `shouldBe` Right "\14"
        parse parseStr' "tests" "\"\\u{f}\"" `shouldBe` Right "\15"
        parse parseStr' "tests" "\"\\u{10}\"" `shouldBe` Right "\16"
        parse parseStr' "tests" "\"\\u{11}\"" `shouldBe` Right "\17"
        parse parseStr' "tests" "\"\\u{12}\"" `shouldBe` Right "\18"
        parse parseStr' "tests" "\"\\u{13}\"" `shouldBe` Right "\19"
        parse parseStr' "tests" "\"\\u{14}\"" `shouldBe` Right "\20"
        parse parseStr' "tests" "\"\\u{15}\"" `shouldBe` Right "\21"
        parse parseStr' "tests" "\"\\u{16}\"" `shouldBe` Right "\22"
        parse parseStr' "tests" "\"\\u{17}\"" `shouldBe` Right "\23"
        parse parseStr' "tests" "\"\\u{18}\"" `shouldBe` Right "\24"
        parse parseStr' "tests" "\"\\u{19}\"" `shouldBe` Right "\25"
        parse parseStr' "tests" "\"\\u{1a}\"" `shouldBe` Right "\26"
        parse parseStr' "tests" "\"\\u{1b}\"" `shouldBe` Right "\27"
        parse parseStr' "tests" "\"\\u{1c}\"" `shouldBe` Right "\28"
        parse parseStr' "tests" "\"\\u{1d}\"" `shouldBe` Right "\29"
        parse parseStr' "tests" "\"\\u{1e}\"" `shouldBe` Right "\30"
        parse parseStr' "tests" "\"\\u{1f}\"" `shouldBe` Right "\31"
        parse parseStr' "tests" "\"\\u{7f}\"" `shouldBe` Right "\127"

unparseStrSpec :: Spec
unparseStrSpec = describe "unparseStr" $ do
    it "unparses string without excaping (ascii)" $ do
        let s = noEscapeChars
        unparseStr' s `shouldBe` ("\"" ++ s ++ "\"")

    it "unparses string without excaping (accents)" $ do
        let s = accentChars
        unparseStr' s `shouldBe` ("\"" ++ s ++ "\"")

    it "unparses string with escaping (non-unicode)" $ do
        unparseStr' "\"" `shouldBe` "\"\\\"\""
        unparseStr' "\\" `shouldBe` "\"\\\\\""
        unparseStr' "/" `shouldBe` "\"\\/\""
        unparseStr' "\b" `shouldBe` "\"\\b\""
        unparseStr' "\f" `shouldBe` "\"\\f\""
        unparseStr' "\n" `shouldBe` "\"\\n\""
        unparseStr' "\r" `shouldBe` "\"\\r\""
        unparseStr' "\t" `shouldBe` "\"\\t\""

    it "unparses string with escaping (unicode)" $ do
        unparseStr' "\0" `shouldBe` "\"\\u{0}\""
        unparseStr' "\1" `shouldBe` "\"\\u{1}\""
        unparseStr' "\2" `shouldBe` "\"\\u{2}\""
        unparseStr' "\3" `shouldBe` "\"\\u{3}\""
        unparseStr' "\4" `shouldBe` "\"\\u{4}\""
        unparseStr' "\5" `shouldBe` "\"\\u{5}\""
        unparseStr' "\6" `shouldBe` "\"\\u{6}\""
        unparseStr' "\7" `shouldBe` "\"\\u{7}\""
        unparseStr' "\8" `shouldBe` "\"\\b\""
        unparseStr' "\9" `shouldBe` "\"\\t\""
        unparseStr' "\10" `shouldBe` "\"\\n\""
        unparseStr' "\11" `shouldBe` "\"\\u{b}\""
        unparseStr' "\12" `shouldBe` "\"\\f\""
        unparseStr' "\13" `shouldBe` "\"\\r\""
        unparseStr' "\14" `shouldBe` "\"\\u{e}\""
        unparseStr' "\15" `shouldBe` "\"\\u{f}\""
        unparseStr' "\16" `shouldBe` "\"\\u{10}\""
        unparseStr' "\17" `shouldBe` "\"\\u{11}\""
        unparseStr' "\18" `shouldBe` "\"\\u{12}\""
        unparseStr' "\19" `shouldBe` "\"\\u{13}\""
        unparseStr' "\20" `shouldBe` "\"\\u{14}\""
        unparseStr' "\21" `shouldBe` "\"\\u{15}\""
        unparseStr' "\22" `shouldBe` "\"\\u{16}\""
        unparseStr' "\23" `shouldBe` "\"\\u{17}\""
        unparseStr' "\24" `shouldBe` "\"\\u{18}\""
        unparseStr' "\25" `shouldBe` "\"\\u{19}\""
        unparseStr' "\26" `shouldBe` "\"\\u{1a}\""
        unparseStr' "\27" `shouldBe` "\"\\u{1b}\""
        unparseStr' "\28" `shouldBe` "\"\\u{1c}\""
        unparseStr' "\29" `shouldBe` "\"\\u{1d}\""
        unparseStr' "\30" `shouldBe` "\"\\u{1e}\""
        unparseStr' "\31" `shouldBe` "\"\\u{1f}\""
        unparseStr' "\127" `shouldBe` "\"\\u{7f}\""

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
        forM_ [[c] | c <- "\b\f\n\t\b\0\31\127"] $ \s@[c] -> do
            leftAsStr (parse parseChar "tests" s) `shouldContain` ("Unprintable characters must be escaped: " ++ unparseChar c)

    it "rejects char with unsupported escape sequence" $ do
        forM_ ['\\':[c] | c <- "acdeghijklmopqsvwxyz"] $ \s -> do
            leftAsStr (parse parseChar "tests" s) `shouldContain` ("Unsupported escape sequence: " ++ s)

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
        leftAsStr (parse parseChar "tests" "\\u{110000}") `shouldContain` "Hex codepoint out of range: 110000"
        leftAsStr (parse parseChar "tests" "\\u{110001}") `shouldContain` "Hex codepoint out of range: 110001"

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
            leftAsStr (parse (parseHexCodepoint $ return "110000") "tests" "") `shouldContain` "Hex codepoint out of range: 110000"
            leftAsStr (parse (parseHexCodepoint $ return "110001") "tests" "") `shouldContain` "Hex codepoint out of range: 110001"

        it "rejects out-of-bounds (prop)" $ do
            property $ \(InvalidCodepoint i) -> do
                let s = showHex i ""
                leftAsStr (parse (parseHexCodepoint $ return $ f s) "tests" "") `shouldContain` ("Hex codepoint out of range: " ++ f s)

-- Utils
noEscapeChars = digits ++ lettersUpper ++ lettersLower ++ symbols
doubleQuoteChar = '"'
backslashChar = '\\'
solidusChar = '/'

unparseStr' = unparseStr . Str
parseStr' = do Str s <- parseStr; return s

instance Arbitrary Str where arbitrary = Str <$> arbitrary

newtype ValidCodepoint = ValidCodepoint Int deriving (Show, Eq)
instance Arbitrary ValidCodepoint where arbitrary = ValidCodepoint <$> chooseInt (0, 0x10FFFF)

newtype InvalidCodepoint = InvalidCodepoint Int deriving (Show, Eq)
instance Arbitrary InvalidCodepoint where arbitrary = InvalidCodepoint <$> chooseInt (0x110000, maxBound)