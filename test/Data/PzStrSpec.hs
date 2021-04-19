module Data.PzStrSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Either
import Data.PzStr
import Text.Parsec

spec :: Spec
spec = do
    parserVsUnparseSpec
    parserSpec
    unparseSpec
    parseCharVsUnparseCharSpec
    parseCharSpec
    unparseCharSpec

parserVsUnparseSpec :: Spec
parserVsUnparseSpec = describe "parse vs unparse" $ do
    it "composes parse with unparse into id" $ do
        property $ \s -> do
            let pzStr = PzStr s
                unparsed = unparse pzStr
            parse parser "tests" unparsed `shouldBe` Right pzStr
            unparse <$> parse parser "tests" unparsed `shouldBe` Right unparsed

parserSpec :: Spec
parserSpec = describe "parserSpec" $ do
    it "rejects non-printable, non-backslash char in string" $ do
        forM_ [noEscapeChars ++ [c] | c <- "\b\f\n\t\b\0\31\127"] $ \s -> do
            isLeft(parse parser "tests" ("\"" ++ s ++ "\"")) `shouldBe` True

    it "rejects unsupported escape sequence" $ do
        forM_ [noEscapeChars ++ ['\\', c] | c <- "acdeghijklmopqsvwxyz"] $ \s -> do
            isLeft(parse parser "tests" ("\"" ++ s ++ "\"")) `shouldBe` True

    it "parses string without escaping (ascii)" $ do
        let s = noEscapeChars
        parse parser "tests" ("\"" ++ s ++ "\"") `shouldBe` Right (PzStr s)

    it "parses string without excaping (accents)" $ do
        let s = accentChars
        parse parser "tests" ("\"" ++ s ++ "\"") `shouldBe` Right (PzStr s)

    it "parses string with escaping (non-unicode)" $ do
        parse parser "tests" "\"\\\"\"" `shouldBe` Right (PzStr "\"")
        parse parser "tests" "\"\\\\\"" `shouldBe` Right (PzStr "\\")
        parse parser "tests" "\"\\/\"" `shouldBe` Right (PzStr "/")
        parse parser "tests" "\"\\b\"" `shouldBe` Right (PzStr "\b")
        parse parser "tests" "\"\\f\"" `shouldBe` Right (PzStr "\f")
        parse parser "tests" "\"\\n\"" `shouldBe` Right (PzStr "\n")
        parse parser "tests" "\"\\r\"" `shouldBe` Right (PzStr "\r")
        parse parser "tests" "\"\\t\"" `shouldBe` Right (PzStr "\t")

    it "parses string with escaping (unicode)" $ do
        parse parser "tests" "\"\\u{0}\"" `shouldBe` Right (PzStr "\0")
        parse parser "tests" "\"\\u{1}\"" `shouldBe` Right (PzStr "\1")
        parse parser "tests" "\"\\u{2}\"" `shouldBe` Right (PzStr "\2")
        parse parser "tests" "\"\\u{3}\"" `shouldBe` Right (PzStr "\3")
        parse parser "tests" "\"\\u{4}\"" `shouldBe` Right (PzStr "\4")
        parse parser "tests" "\"\\u{5}\"" `shouldBe` Right (PzStr "\5")
        parse parser "tests" "\"\\u{6}\"" `shouldBe` Right (PzStr "\6")
        parse parser "tests" "\"\\u{7}\"" `shouldBe` Right (PzStr "\7")
        -- \b
        -- \t
        -- \n
        parse parser "tests" "\"\\u{b}\"" `shouldBe` Right (PzStr "\11")
        -- \f
        -- \r
        parse parser "tests" "\"\\u{e}\"" `shouldBe` Right (PzStr "\14")
        parse parser "tests" "\"\\u{f}\"" `shouldBe` Right (PzStr "\15")
        parse parser "tests" "\"\\u{10}\"" `shouldBe` Right (PzStr "\16")
        parse parser "tests" "\"\\u{11}\"" `shouldBe` Right (PzStr "\17")
        parse parser "tests" "\"\\u{12}\"" `shouldBe` Right (PzStr "\18")
        parse parser "tests" "\"\\u{13}\"" `shouldBe` Right (PzStr "\19")
        parse parser "tests" "\"\\u{14}\"" `shouldBe` Right (PzStr "\20")
        parse parser "tests" "\"\\u{15}\"" `shouldBe` Right (PzStr "\21")
        parse parser "tests" "\"\\u{16}\"" `shouldBe` Right (PzStr "\22")
        parse parser "tests" "\"\\u{17}\"" `shouldBe` Right (PzStr "\23")
        parse parser "tests" "\"\\u{18}\"" `shouldBe` Right (PzStr "\24")
        parse parser "tests" "\"\\u{19}\"" `shouldBe` Right (PzStr "\25")
        parse parser "tests" "\"\\u{1a}\"" `shouldBe` Right (PzStr "\26")
        parse parser "tests" "\"\\u{1b}\"" `shouldBe` Right (PzStr "\27")
        parse parser "tests" "\"\\u{1c}\"" `shouldBe` Right (PzStr "\28")
        parse parser "tests" "\"\\u{1d}\"" `shouldBe` Right (PzStr "\29")
        parse parser "tests" "\"\\u{1e}\"" `shouldBe` Right (PzStr "\30")
        parse parser "tests" "\"\\u{1f}\"" `shouldBe` Right (PzStr "\31")
        parse parser "tests" "\"\\u{7f}\"" `shouldBe` Right (PzStr "\127")

unparseSpec :: Spec
unparseSpec = describe "unparse" $ do
    it "unparses string without excaping (ascii)" $ do
        let s = noEscapeChars
        unparse (PzStr s) `shouldBe` ("\"" ++ s ++ "\"")

    it "unparses string without excaping (accents)" $ do
        let s = accentChars
        unparse (PzStr s) `shouldBe` ("\"" ++ s ++ "\"")

    it "unparses string with escaping (non-unicode)" $ do
        unparse (PzStr "\"") `shouldBe` "\"\\\"\""
        unparse (PzStr "\\") `shouldBe` "\"\\\\\""
        unparse (PzStr "/") `shouldBe` "\"\\/\""
        unparse (PzStr "\b") `shouldBe` "\"\\b\""
        unparse (PzStr "\f") `shouldBe` "\"\\f\""
        unparse (PzStr "\n") `shouldBe` "\"\\n\""
        unparse (PzStr "\r") `shouldBe` "\"\\r\""
        unparse (PzStr "\t") `shouldBe` "\"\\t\""

    it "unparses string with escaping (unicode)" $ do
        unparse (PzStr "\0") `shouldBe` "\"\\u{0}\""
        unparse (PzStr "\1") `shouldBe` "\"\\u{1}\""
        unparse (PzStr "\2") `shouldBe` "\"\\u{2}\""
        unparse (PzStr "\3") `shouldBe` "\"\\u{3}\""
        unparse (PzStr "\4") `shouldBe` "\"\\u{4}\""
        unparse (PzStr "\5") `shouldBe` "\"\\u{5}\""
        unparse (PzStr "\6") `shouldBe` "\"\\u{6}\""
        unparse (PzStr "\7") `shouldBe` "\"\\u{7}\""
        -- \b
        -- \t
        -- \n
        unparse (PzStr "\11") `shouldBe` "\"\\u{b}\""
        -- \f
        -- \r
        unparse (PzStr "\14") `shouldBe` "\"\\u{e}\""
        unparse (PzStr "\15") `shouldBe` "\"\\u{f}\""
        unparse (PzStr "\16") `shouldBe` "\"\\u{10}\""
        unparse (PzStr "\17") `shouldBe` "\"\\u{11}\""
        unparse (PzStr "\18") `shouldBe` "\"\\u{12}\""
        unparse (PzStr "\19") `shouldBe` "\"\\u{13}\""
        unparse (PzStr "\20") `shouldBe` "\"\\u{14}\""
        unparse (PzStr "\21") `shouldBe` "\"\\u{15}\""
        unparse (PzStr "\22") `shouldBe` "\"\\u{16}\""
        unparse (PzStr "\23") `shouldBe` "\"\\u{17}\""
        unparse (PzStr "\24") `shouldBe` "\"\\u{18}\""
        unparse (PzStr "\25") `shouldBe` "\"\\u{19}\""
        unparse (PzStr "\26") `shouldBe` "\"\\u{1a}\""
        unparse (PzStr "\27") `shouldBe` "\"\\u{1b}\""
        unparse (PzStr "\28") `shouldBe` "\"\\u{1c}\""
        unparse (PzStr "\29") `shouldBe` "\"\\u{1d}\""
        unparse (PzStr "\30") `shouldBe` "\"\\u{1e}\""
        unparse (PzStr "\31") `shouldBe` "\"\\u{1f}\""
        unparse (PzStr "\127") `shouldBe` "\"\\u{7f}\""

parseCharVsUnparseCharSpec :: Spec
parseCharVsUnparseCharSpec = describe "parseChar vs unparseChar" $ do
    it "composes parseChar with unparseChar into id" $ do
        property $ \c -> do
            parse parseChar "tests" (unparseChar c) `shouldBe` Right c
            unparseChar <$> parse parseChar "tests" (unparseChar c) `shouldBe` Right (unparseChar c)

parseCharSpec :: Spec
parseCharSpec = describe "parseCharSpec" $ do
    it "rejects non-printable, non-backslash char" $ do
        forM_ [[c] | c <- "\b\f\n\t\b\0\31\127"] $ \s -> do
            isLeft(parse parseChar "tests" s) `shouldBe` True

    it "rejects char with unsupported escape sequence" $ do
        forM_ ['\\':[c] | c <- "acdeghijklmopqsvwxyz"] $ \s -> do
            isLeft(parse parseChar "tests" s) `shouldBe` True

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


-- Utils
digits :: [Char]
digits = ['0'..'9']

lettersUpper :: [Char]
lettersUpper = ['A'..'Z']

lettersLower :: [Char]
lettersLower = ['a'..'z']

symbols :: [Char]
symbols = " !#$%&'()*+,-.:;<=>?@[]^_`{|}~"

doubleQuoteChar :: Char
doubleQuoteChar = '"'

backslashChar :: Char
backslashChar = '\\'

solidusChar :: Char
solidusChar = '/'

noEscapeChars :: String
noEscapeChars = digits ++ lettersUpper ++ lettersLower ++ symbols

accentChars :: String
accentChars = "àâäĉèéêëĝĥîïĵôöŝùûüŵŷÿ"