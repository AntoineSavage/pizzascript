module Data.IdentSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Either
import Data.Ident
import Text.Parsec
import Utils.ArbWithDepth

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
        forM_ (digits ++ symbols ++ escapees) $ \f -> do
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

-- Utils
digits = ['0'..'9']
lettersUpper = ['A'..'Z']
lettersLower = ['a'..'z']
symbols = " !#$%&'()*+,-.:;<=>?@[]^`{|}~"
accentChars = "àâäĉèéêëĝĥîïĵôöŝùûüŵŷÿ"
escapees = "\"\\\b\f\n\r\t"
underscore = '_'

validFirsts = underscore : lettersUpper ++ lettersLower ++ accentChars
validNexts = underscore : digits ++ lettersUpper ++ lettersLower ++ accentChars

instance Arbitrary Ident where
    arbitrary = do
        first <- elements validFirsts
        nexts <- chooseInt (0, 10) >>= flip vectorOf (elements validNexts)
        return $ Ident $ first : nexts

instance ArbWithDepth Ident where arbWithDepth _ = arbitrary