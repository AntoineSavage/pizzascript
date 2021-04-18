module Data.PzStrSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Either
import Data.PzStr
import Text.Parsec
import Utils

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