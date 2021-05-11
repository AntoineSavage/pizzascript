module Data.IdentSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Either
import Data.Ident
import TestUtils
import Text.Parsec

spec :: Spec
spec = do
    parseIdentVsUnparseIdentSpec
    parseIdentSpec
    unparseIdentSpec

parseIdentVsUnparseIdentSpec :: Spec
parseIdentVsUnparseIdentSpec = describe "parseIdent vs unparseIdent" $ do
    it "composes parseIdent and unparseIdent into id" $ do
        property $ \ident -> do
            let s = unparseIdent ident
            parse parseIdent "tests" s `shouldBe` Right ident
            unparseIdent <$> parse parseIdent "tests" s `shouldBe` Right s

parseIdentSpec :: Spec
parseIdentSpec = describe "parseIdent" $ do
    it "rejects invalid first" $ do
        forM_ invalidFirsts $ \f -> do
            let s = f : validNexts
            isLeft (parse parseIdent "tests" s) `shouldBe` True

    it "parses successfully" $ do
        forM_ validFirsts $ \f -> do
            let s = f : validNexts
            parse parseIdent "tests" s `shouldBe` Right (Ident s)

unparseIdentSpec :: Spec
unparseIdentSpec = describe "unparseIdent" $ do
    it "returns input string" $ do
        property $ \s -> do
            unparseIdent (Ident s) `shouldBe` s
