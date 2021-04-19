module Data.PzStrSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Either
import Data.PzStr
import Text.Parsec

spec :: Spec
spec = do
    parseSpec
    unparseSpec

parseSpec :: Spec
parseSpec = describe "parse" $ do
    it "parses unescaped strings" $ do
        forM_ ["", digits, lettersUpper, lettersLower, symbols, [underscore]] $ \s -> do
                parse parser "tests" ("\"" ++ s ++ "\"") `shouldBe` Right (PzStr s)

unparseSpec :: Spec
unparseSpec = describe "unparse" $ do
    it "unparses unescaped strings" $ do
        forM_ ["", digits, lettersUpper, lettersLower, symbols, [underscore]] $ \s -> do
            unparse (PzStr s) `shouldBe` "\"" ++ s ++ "\""

digits :: [Char]
digits = ['0'..'9']


lettersUpper :: [Char]
lettersUpper = ['A'..'Z']

lettersLower :: [Char]
lettersLower = ['a'..'z']

doubleQuote :: Char
doubleQuote = '"'

backslash :: Char
backslash = '\\'

underscore :: Char
underscore = '_'

symbols :: [Char]
symbols = " !#$%&'()*+,-./:;<=>?@[]^`{|}~"