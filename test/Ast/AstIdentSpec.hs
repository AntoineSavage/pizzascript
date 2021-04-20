module Ast.AstIdentSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Either
import Ast.AstIdent
import Text.Parsec

spec :: Spec
spec = do
    parseVsUnparseSpec
    parseSpec
    unparseSpec
    parsePartVsUnparsePartSpec
    parsePartSpec
    unparsePartSpec

parseVsUnparseSpec :: Spec
parseVsUnparseSpec = describe "parse vs unparse" $ do
    it "composes parse and unparse into id" $ do
        property $ \ident -> do
            let s = unparse ident
            parse parser "tests" s `shouldBe` Right ident
            unparse <$> parse parser "tests" s `shouldBe` Right (unparse ident)

parseSpec :: Spec
parseSpec = describe "parse" $ do
    it "rejects empty string" $ do
        isLeft(parse parser "tests" "") `shouldBe` True

    it "parse single part" $ do
        property $ \(AstIdentPart f1 ns1) -> do
            let s = f1 : ns1
            parse parser "tests" s `shouldBe` Right (AstIdent (AstIdentPart f1 ns1) [])

    it "parse two parts" $ do
        property $ \(AstIdentPart f1 ns1) (AstIdentPart f2 ns2) -> do
            let s = f1 : ns1 ++ "." ++ [f2] ++ ns2
            parse parser "tests" s `shouldBe` Right (AstIdent (AstIdentPart f1 ns1) [AstIdentPart f2 ns2])

    it "parse two parts" $ do
        property $ \(AstIdentPart f1 ns1) (AstIdentPart f2 ns2) (AstIdentPart f3 ns3) -> do
            let s = f1 : ns1 ++ "." ++ [f2] ++ ns2 ++ "." ++ [f3] ++ ns3
            parse parser "tests" s `shouldBe` Right (AstIdent (AstIdentPart f1 ns1) [AstIdentPart f2 ns2, AstIdentPart f3 ns3])

unparseSpec :: Spec
unparseSpec = describe "unparse" $ do
    it "returns single part for singleton list" $ do
        property $ \(AstIdentPart f1 ns1) -> do
            let s = f1 : ns1
            unparse (AstIdent (AstIdentPart f1 ns1) []) `shouldBe` s

    it "intercalates dots between two parts" $ do
        property $ \(AstIdentPart f1 ns1) (AstIdentPart f2 ns2) -> do
            let s = f1 : ns1 ++ "." ++ [f2] ++ ns2
            unparse (AstIdent (AstIdentPart f1 ns1) [AstIdentPart f2 ns2]) `shouldBe` s

    it "intercalates dots between three parts" $ do
        property $ \(AstIdentPart f1 ns1) (AstIdentPart f2 ns2) (AstIdentPart f3 ns3) -> do
            let s = f1 : ns1 ++ "." ++ [f2] ++ ns2 ++ "." ++ [f3] ++ ns3
            unparse (AstIdent (AstIdentPart f1 ns1) [AstIdentPart f2 ns2, AstIdentPart f3 ns3]) `shouldBe` s

parsePartVsUnparsePartSpec :: Spec
parsePartVsUnparsePartSpec = describe "parsePart vs unparsePart" $ do
    it "composes parsePart and unparsePart into id" $ do
        property $ \(AstIdentPart f ns) -> do
            let s = f : ns
            parse parsePart "tests" s `shouldBe` Right (AstIdentPart f ns)
            unparsePart <$> parse parsePart "tests" s `shouldBe` Right (unparsePart $ AstIdentPart f ns)

parsePartSpec :: Spec
parsePartSpec = describe "parsePart" $ do
    it "rejects invalid first" $ do
        forM_ invalidFirsts $ \f -> do
            let s = f : validNexts
            isLeft (parse parsePart "tests" s) `shouldBe` True

    it "parses successfully" $ do
        forM_ validFirsts $ \f -> do
            let s = f : validNexts
            parse parsePart "tests" s `shouldBe` Right (AstIdentPart f validNexts)

unparsePartSpec :: Spec
unparsePartSpec = describe "unparsePart" $ do
    it "returns the input ident part" $ do
        property $ \f ns -> do
            unparsePart (AstIdentPart f ns) `shouldBe` f : ns

-- Utils
digits :: [Char]
digits = ['0'..'9']

lettersUpper :: [Char]
lettersUpper = ['A'..'Z']

lettersLower :: [Char]
lettersLower = ['a'..'z']

symbols :: [Char]
symbols = " !#$%&'()*+,-./:;<=>?@[]^`{|}~"

underscore :: Char
underscore = '_'

accentChars :: [Char]
accentChars = "àâäĉèéêëĝĥîïĵôöŝùûüŵŷÿ"

escapees :: [Char]
escapees = "\"\\\b\f\n\r\t"

validFirsts :: [Char]
validFirsts = underscore : lettersUpper ++ lettersLower ++ accentChars

invalidFirsts :: [Char]
invalidFirsts = digits ++ symbols ++ escapees

validNexts :: [Char]
validNexts = underscore : digits ++ lettersUpper ++ lettersLower ++ accentChars

instance Arbitrary AstIdent where
    arbitrary = liftM2 AstIdent arbitrary
        $ chooseInt (0, 10) >>= vector

instance Arbitrary AstIdentPart where
    arbitrary = liftM2 AstIdentPart (elements validFirsts)
        $ chooseInt (0, 10) >>= flip vectorOf (elements validNexts)