module Data.PzStrSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Either
import Data.PzStr
import Text.Parsec

spec :: Spec
spec = do
    -- parseVsUnparseSpec
    -- parseSpec
    -- unparseSpec
    -- parseCharVsUnparseCharSpec
    parseCharSpec
    unparseCharSpec
{-

parseVsUnparseSpec :: Spec
parseVsUnparseSpec = describe "parse vs unparse" $ do
    it "composes parse and unparse into id" $ do
        property $ \s -> do
            parse parser "tests" (unparse $ PzStr s) `shouldBe` Right (PzStr s)
            unparse <$> parse parser "tests" (unparse $ PzStr s) `shouldBe` Right (unparse $ PzStr s)

parseSpec :: Spec
parseSpec = describe "parse" $ do
    it "parses parseCharSpecd strings" $ do
        forM_ ["", digits, lettersUpper, lettersLower, symbols, [underscoreChar, spaceChar]] $ \s -> do
                parse parser "tests" ("\"" ++ s ++ "\"") `shouldBe` Right (PzStr s)

    it "parses escaped strings" $ do
        parse parser "tests" "\"\\\"\\\\\\a\\b\\d\\e\\f\\n\\r\\s\\t\\v\""
            `shouldBe` Right (PzStr "\"\\\a\b\x7F\x1B\f\n\r\x20\t\v")

    it "rejects invalid escape sequence" $ do
        isLeft (parse parser "tests" "\"\\y\"") `shouldBe` True

unparseSpec :: Spec
unparseSpec = describe "unparse" $ do
    it "unparses strings without escaping" $ do
        forM_ ["", digits, lettersUpper, lettersLower, symbols, [underscoreChar]] $ \s -> do
            unparse (PzStr s) `shouldBe` "\"" ++ s ++ "\""

    it "unparses strings with escaping" $ do
        unparse (PzStr  "\"\\\a\b\x7F\x1B\f\n\r\x20\t\v")
            `shouldBe` "\"\\\"\\\\\\a\\b\\d\\e\\f\\n\\r\\s\\t\\v\""
-}

parseCharSpec :: Spec
parseCharSpec = describe "parseCharSpec" $ do
    it "rejects non-printable, non-backslash chars" $ do
        forM_ [[c] | c <- "\b\f\n\t\b\0\31\127"] $ \s -> do
            isLeft(parse parseChar "tests" s) `shouldBe` True

    it "rejects unsupported escape sequences" $ do
        forM_ ['\\':[c] | c <- "acdeghijklmopqsvwxyz"] $ \s -> do
            isLeft(parse parseChar "tests" s) `shouldBe` True

    it "parses chars without escaping (ascii)" $ do
        forM_ (digits
                ++ lettersUpper ++ lettersLower
                ++ symbols ++ [underscoreChar]
                ) $ \c -> do
            parse parseChar "tests" [c] `shouldBe` Right c

    it "parses chars without excaping (accents)" $ do
        forM_ "àâäĉèéêëĝĥîïĵôöŝùûüŵŷÿ" $ \c -> do
            parse parseChar "tests" [c] `shouldBe` Right c

    it "parses chars with escaping (non-unicode)" $ do
        parse parseChar "tests" "\\\"" `shouldBe` Right '"'
        parse parseChar "tests" "\\\\" `shouldBe` Right '\\'
        parse parseChar "tests" "\\/" `shouldBe` Right '/'
        parse parseChar "tests" "\\b" `shouldBe` Right '\b'
        parse parseChar "tests" "\\f" `shouldBe` Right '\f'
        parse parseChar "tests" "\\n" `shouldBe` Right '\n'
        parse parseChar "tests" "\\r" `shouldBe` Right '\r'
        parse parseChar "tests" "\\t" `shouldBe` Right '\t'

    it "parses chars with escaping (unicode)" $ do
        parse parseChar "tests" "\\u0000" `shouldBe` Right '\0'
        parse parseChar "tests" "\\u0001" `shouldBe` Right '\1'
        parse parseChar "tests" "\\u0002" `shouldBe` Right '\2'
        parse parseChar "tests" "\\u0003" `shouldBe` Right '\3'
        parse parseChar "tests" "\\u0004" `shouldBe` Right '\4'
        parse parseChar "tests" "\\u0005" `shouldBe` Right '\5'
        parse parseChar "tests" "\\u0006" `shouldBe` Right '\6'
        parse parseChar "tests" "\\u0007" `shouldBe` Right '\7'
        -- \b
        -- \t
        -- \n
        parse parseChar "tests" "\\u000b" `shouldBe` Right '\11'
        -- \f
        -- \r
        parse parseChar "tests" "\\u000e" `shouldBe` Right '\14'
        parse parseChar "tests" "\\u000f" `shouldBe` Right '\15'
        parse parseChar "tests" "\\u0010" `shouldBe` Right '\16'
        parse parseChar "tests" "\\u0011" `shouldBe` Right '\17'
        parse parseChar "tests" "\\u0012" `shouldBe` Right '\18'
        parse parseChar "tests" "\\u0013" `shouldBe` Right '\19'
        parse parseChar "tests" "\\u0014" `shouldBe` Right '\20'
        parse parseChar "tests" "\\u0015" `shouldBe` Right '\21'
        parse parseChar "tests" "\\u0016" `shouldBe` Right '\22'
        parse parseChar "tests" "\\u0017" `shouldBe` Right '\23'
        parse parseChar "tests" "\\u0018" `shouldBe` Right '\24'
        parse parseChar "tests" "\\u0019" `shouldBe` Right '\25'
        parse parseChar "tests" "\\u001a" `shouldBe` Right '\26'
        parse parseChar "tests" "\\u001b" `shouldBe` Right '\27'
        parse parseChar "tests" "\\u001c" `shouldBe` Right '\28'
        parse parseChar "tests" "\\u001d" `shouldBe` Right '\29'
        parse parseChar "tests" "\\u001e" `shouldBe` Right '\30'
        parse parseChar "tests" "\\u001f" `shouldBe` Right '\31'
        parse parseChar "tests" "\\u007f" `shouldBe` Right '\127'

unparseCharSpec :: Spec
unparseCharSpec = describe "unparseChar" $ do
    it "unparses chars without excaping (ascii)" $ do
        forM_ (digits
                ++ lettersUpper ++ lettersLower
                ++ symbols ++ [underscoreChar]
                ) $ \c -> do
            unparseChar c `shouldBe` [c]

    it "unparses chars without excaping (accents)" $ do
        forM_ "àâäĉèéêëĝĥîïĵôöŝùûüŵŷÿ" $ \c -> do
            unparseChar c `shouldBe` [c]

    it "unparses chars with escaping (non-unicode)" $ do
        unparseChar '"' `shouldBe` "\""
        unparseChar '\\' `shouldBe` "\\\\"
        unparseChar '/' `shouldBe` "\\/"
        unparseChar '\b' `shouldBe` "\\b"
        unparseChar '\f' `shouldBe` "\\f"
        unparseChar '\n' `shouldBe` "\\n"
        unparseChar '\r' `shouldBe` "\\r"
        unparseChar '\t' `shouldBe` "\\t"

    it "unparses chars with escaping (unicode)" $ do
        unparseChar '\0' `shouldBe` "\\u0000"
        unparseChar '\1' `shouldBe` "\\u0001"
        unparseChar '\2' `shouldBe` "\\u0002"
        unparseChar '\3' `shouldBe` "\\u0003"
        unparseChar '\4' `shouldBe` "\\u0004"
        unparseChar '\5' `shouldBe` "\\u0005"
        unparseChar '\6' `shouldBe` "\\u0006"
        unparseChar '\7' `shouldBe` "\\u0007"
        -- \b
        -- \t
        -- \n
        unparseChar '\11' `shouldBe` "\\u000b"
        -- \f
        -- \r
        unparseChar '\14' `shouldBe` "\\u000e"
        unparseChar '\15' `shouldBe` "\\u000f"
        unparseChar '\16' `shouldBe` "\\u0010"
        unparseChar '\17' `shouldBe` "\\u0011"
        unparseChar '\18' `shouldBe` "\\u0012"
        unparseChar '\19' `shouldBe` "\\u0013"
        unparseChar '\20' `shouldBe` "\\u0014"
        unparseChar '\21' `shouldBe` "\\u0015"
        unparseChar '\22' `shouldBe` "\\u0016"
        unparseChar '\23' `shouldBe` "\\u0017"
        unparseChar '\24' `shouldBe` "\\u0018"
        unparseChar '\25' `shouldBe` "\\u0019"
        unparseChar '\26' `shouldBe` "\\u001a"
        unparseChar '\27' `shouldBe` "\\u001b"
        unparseChar '\28' `shouldBe` "\\u001c"
        unparseChar '\29' `shouldBe` "\\u001d"
        unparseChar '\30' `shouldBe` "\\u001e"
        unparseChar '\31' `shouldBe` "\\u001f"
        unparseChar '\127' `shouldBe` "\\u007f"


-- Utils
digits :: [Char]
digits = ['0'..'9']

lettersUpper :: [Char]
lettersUpper = ['A'..'Z']

lettersLower :: [Char]
lettersLower = ['a'..'z']

symbols :: [Char]
symbols = " !#$%&'()*+,-.:;<=>?@[]^`{|}~"

doubleQuoteChar :: Char
doubleQuoteChar = '"'

backslashChar :: Char
backslashChar = '\\'

solidusChar :: Char
solidusChar = '/'

underscoreChar :: Char
underscoreChar = '_'