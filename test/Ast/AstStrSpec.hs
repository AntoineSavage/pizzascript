module Ast.AstStrSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Either
import Ast.AstStr
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
            let astStr = AstStr s
                unparsed = unparse astStr
            parse parser "tests" unparsed `shouldBe` Right astStr
            unparse <$> parse parser "tests" unparsed `shouldBe` Right unparsed

parserSpec :: Spec
parserSpec = describe "parserSpec" $ do
    it "rejects empty string" $ do
        isLeft(parse parser "tests" "") `shouldBe` True

    it "rejects non-printable, non-backslash char in string" $ do
        forM_ [noEscapeChars ++ [c] | c <- "\b\f\n\t\b\0\31\127"] $ \s -> do
            isLeft(parse parser "tests" ("\"" ++ s ++ "\"")) `shouldBe` True

    it "rejects unsupported escape sequence" $ do
        forM_ [noEscapeChars ++ ['\\', c] | c <- "acdeghijklmopqsvwxyz"] $ \s -> do
            isLeft(parse parser "tests" ("\"" ++ s ++ "\"")) `shouldBe` True

    it "parses string without escaping (ascii)" $ do
        let s = noEscapeChars
        parse parser "tests" ("\"" ++ s ++ "\"") `shouldBe` Right (AstStr s)

    it "parses string without excaping (accents)" $ do
        let s = accentChars
        parse parser "tests" ("\"" ++ s ++ "\"") `shouldBe` Right (AstStr s)

    it "parses string with escaping (non-unicode)" $ do
        parse parser "tests" "\"\\\"\"" `shouldBe` Right (AstStr "\"")
        parse parser "tests" "\"\\\\\"" `shouldBe` Right (AstStr "\\")
        parse parser "tests" "\"\\/\"" `shouldBe` Right (AstStr "/")
        parse parser "tests" "\"\\b\"" `shouldBe` Right (AstStr "\b")
        parse parser "tests" "\"\\f\"" `shouldBe` Right (AstStr "\f")
        parse parser "tests" "\"\\n\"" `shouldBe` Right (AstStr "\n")
        parse parser "tests" "\"\\r\"" `shouldBe` Right (AstStr "\r")
        parse parser "tests" "\"\\t\"" `shouldBe` Right (AstStr "\t")

    it "parses string with escaping (unicode)" $ do
        parse parser "tests" "\"\\u{0}\"" `shouldBe` Right (AstStr "\0")
        parse parser "tests" "\"\\u{1}\"" `shouldBe` Right (AstStr "\1")
        parse parser "tests" "\"\\u{2}\"" `shouldBe` Right (AstStr "\2")
        parse parser "tests" "\"\\u{3}\"" `shouldBe` Right (AstStr "\3")
        parse parser "tests" "\"\\u{4}\"" `shouldBe` Right (AstStr "\4")
        parse parser "tests" "\"\\u{5}\"" `shouldBe` Right (AstStr "\5")
        parse parser "tests" "\"\\u{6}\"" `shouldBe` Right (AstStr "\6")
        parse parser "tests" "\"\\u{7}\"" `shouldBe` Right (AstStr "\7")
        -- \b
        -- \t
        -- \n
        parse parser "tests" "\"\\u{b}\"" `shouldBe` Right (AstStr "\11")
        -- \f
        -- \r
        parse parser "tests" "\"\\u{e}\"" `shouldBe` Right (AstStr "\14")
        parse parser "tests" "\"\\u{f}\"" `shouldBe` Right (AstStr "\15")
        parse parser "tests" "\"\\u{10}\"" `shouldBe` Right (AstStr "\16")
        parse parser "tests" "\"\\u{11}\"" `shouldBe` Right (AstStr "\17")
        parse parser "tests" "\"\\u{12}\"" `shouldBe` Right (AstStr "\18")
        parse parser "tests" "\"\\u{13}\"" `shouldBe` Right (AstStr "\19")
        parse parser "tests" "\"\\u{14}\"" `shouldBe` Right (AstStr "\20")
        parse parser "tests" "\"\\u{15}\"" `shouldBe` Right (AstStr "\21")
        parse parser "tests" "\"\\u{16}\"" `shouldBe` Right (AstStr "\22")
        parse parser "tests" "\"\\u{17}\"" `shouldBe` Right (AstStr "\23")
        parse parser "tests" "\"\\u{18}\"" `shouldBe` Right (AstStr "\24")
        parse parser "tests" "\"\\u{19}\"" `shouldBe` Right (AstStr "\25")
        parse parser "tests" "\"\\u{1a}\"" `shouldBe` Right (AstStr "\26")
        parse parser "tests" "\"\\u{1b}\"" `shouldBe` Right (AstStr "\27")
        parse parser "tests" "\"\\u{1c}\"" `shouldBe` Right (AstStr "\28")
        parse parser "tests" "\"\\u{1d}\"" `shouldBe` Right (AstStr "\29")
        parse parser "tests" "\"\\u{1e}\"" `shouldBe` Right (AstStr "\30")
        parse parser "tests" "\"\\u{1f}\"" `shouldBe` Right (AstStr "\31")
        parse parser "tests" "\"\\u{7f}\"" `shouldBe` Right (AstStr "\127")

unparseSpec :: Spec
unparseSpec = describe "unparse" $ do
    it "unparses string without excaping (ascii)" $ do
        let s = noEscapeChars
        unparse (AstStr s) `shouldBe` ("\"" ++ s ++ "\"")

    it "unparses string without excaping (accents)" $ do
        let s = accentChars
        unparse (AstStr s) `shouldBe` ("\"" ++ s ++ "\"")

    it "unparses string with escaping (non-unicode)" $ do
        unparse (AstStr "\"") `shouldBe` "\"\\\"\""
        unparse (AstStr "\\") `shouldBe` "\"\\\\\""
        unparse (AstStr "/") `shouldBe` "\"\\/\""
        unparse (AstStr "\b") `shouldBe` "\"\\b\""
        unparse (AstStr "\f") `shouldBe` "\"\\f\""
        unparse (AstStr "\n") `shouldBe` "\"\\n\""
        unparse (AstStr "\r") `shouldBe` "\"\\r\""
        unparse (AstStr "\t") `shouldBe` "\"\\t\""

    it "unparses string with escaping (unicode)" $ do
        unparse (AstStr "\0") `shouldBe` "\"\\u{0}\""
        unparse (AstStr "\1") `shouldBe` "\"\\u{1}\""
        unparse (AstStr "\2") `shouldBe` "\"\\u{2}\""
        unparse (AstStr "\3") `shouldBe` "\"\\u{3}\""
        unparse (AstStr "\4") `shouldBe` "\"\\u{4}\""
        unparse (AstStr "\5") `shouldBe` "\"\\u{5}\""
        unparse (AstStr "\6") `shouldBe` "\"\\u{6}\""
        unparse (AstStr "\7") `shouldBe` "\"\\u{7}\""
        -- \b
        -- \t
        -- \n
        unparse (AstStr "\11") `shouldBe` "\"\\u{b}\""
        -- \f
        -- \r
        unparse (AstStr "\14") `shouldBe` "\"\\u{e}\""
        unparse (AstStr "\15") `shouldBe` "\"\\u{f}\""
        unparse (AstStr "\16") `shouldBe` "\"\\u{10}\""
        unparse (AstStr "\17") `shouldBe` "\"\\u{11}\""
        unparse (AstStr "\18") `shouldBe` "\"\\u{12}\""
        unparse (AstStr "\19") `shouldBe` "\"\\u{13}\""
        unparse (AstStr "\20") `shouldBe` "\"\\u{14}\""
        unparse (AstStr "\21") `shouldBe` "\"\\u{15}\""
        unparse (AstStr "\22") `shouldBe` "\"\\u{16}\""
        unparse (AstStr "\23") `shouldBe` "\"\\u{17}\""
        unparse (AstStr "\24") `shouldBe` "\"\\u{18}\""
        unparse (AstStr "\25") `shouldBe` "\"\\u{19}\""
        unparse (AstStr "\26") `shouldBe` "\"\\u{1a}\""
        unparse (AstStr "\27") `shouldBe` "\"\\u{1b}\""
        unparse (AstStr "\28") `shouldBe` "\"\\u{1c}\""
        unparse (AstStr "\29") `shouldBe` "\"\\u{1d}\""
        unparse (AstStr "\30") `shouldBe` "\"\\u{1e}\""
        unparse (AstStr "\31") `shouldBe` "\"\\u{1f}\""
        unparse (AstStr "\127") `shouldBe` "\"\\u{7f}\""

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

noEscapeChars :: [Char]
noEscapeChars = digits ++ lettersUpper ++ lettersLower ++ symbols

accentChars :: String
accentChars = "àâäĉèéêëĝĥîïĵôöŝùûüŵŷÿ"

instance Arbitrary AstStr where
    arbitrary = AstStr <$> (chooseInt (0, 20) >>= vector)