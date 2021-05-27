module Ops.SymbSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Exception
import Control.Monad
import Data.Either
import Ops.Nat
import Ops.NatSpec
import Ops.Symb
import TestUtils
import Text.Parsec
import Types.Nat
import Types.Symb
import Types.SymbSpec

spec :: Spec
spec = do
    symbSpec
    parseSymbVsUnparseSymbSpec
    parseSymbSpec
    unparseSymbSpec
    quoteSymbVsUnquoteSymbSpec
    quoteSymbSpec
    unquoteSymbSpec

symbSpec :: Spec
symbSpec = describe "symb" $ do
    it "rejects empty strings" $ do
        evaluate (symb []) `shouldThrow` errorCall "Symbols must contain at least one character"

    it "converts non-string to symb" $ do
        property $ \f ns -> do
            symb (f:ns) `shouldBe` Symb Z f ns

parseSymbVsUnparseSymbSpec :: Spec
parseSymbVsUnparseSymbSpec = describe "parseSymb vs unparseSymb" $ do
    it "composes parseSymb and unparseSymb into id" $ do
        property $ \symb -> do
            let s = unparseSymb symb
            parse parseSymb "tests" s `shouldBe` Right symb
            unparseSymb <$> parse parseSymb "tests" s `shouldBe` Right s

parseSymbSpec :: Spec
parseSymbSpec = describe "parseSymb" $ do
    it "rejects empty string" $ do
        isLeft (parse parseSymb "tests" "") `shouldBe` True

    it "rejects invalid first" $ do
        forM_ (digits ++ symbols ++ escapees) $ \f -> do
            let s = f : validNexts
            isLeft (parse parseSymb "tests" s) `shouldBe` True

    it "parses identifier" $ do
        forM_ validFirsts $ \f -> do
            let s = f : validNexts
            parse parseSymb "tests" s `shouldBe` Right (Symb Z f validNexts)

    it "parses symbol" $ do
        property $ \n (Ident f ns) -> do
            let s = unlen n '\'' ++ [f] ++ ns
            parse parseSymb "tests" s `shouldBe` Right (Symb n f ns)

unparseSymbSpec :: Spec
unparseSymbSpec = describe "unparseSymb" $ do
    it "unparses simplest symbol" $ do
        property $ \f -> do
            unparseSymb (Symb Z f "") `shouldBe` [f]
 
    it "unparses arbitrary symbol" $ do
        property $ \n f ns -> do
            unparseSymb (Symb n f ns) `shouldBe` unlen n '\'' ++ [f] ++ ns

quoteSymbVsUnquoteSymbSpec :: Spec
quoteSymbVsUnquoteSymbSpec = describe "quoteSymb vs unquoteSymb" $ do
    it "composes quoteSymb and unquoteSymb into id" $ do
        property $ \n (Ident f ns) -> do
            let s = Symb n f ns
                s' = Symb (S n) f ns
            unquoteSymb (quoteSymb s) `shouldBe` s
            quoteSymb (unquoteSymb s') `shouldBe` s'

quoteSymbSpec :: Spec
quoteSymbSpec = describe "quoteSymb" $ do
    it "converts symbols to one-more-quoted symbols" $ do
        property $ \n (Ident f ns) -> do
            quoteSymb (Symb n f ns) `shouldBe` Symb (S n) f ns

unquoteSymbSpec :: Spec
unquoteSymbSpec = describe "quoteSymb vs unquoteSymb" $ do
    it "rejects quoted identifiers" $ do
        property $ \(Ident f ns) -> do
            let s = Symb Z f ns
            evaluate (unquoteSymb s) `shouldThrow` errorCall ("Quoted identifier cannot be unquoted: " ++ f:ns)

    it "converts quoted symbols to one-less-quoted symbols" $ do
        property $ \n (Ident f ns) -> do
            unquoteSymb (Symb (S n) f ns) `shouldBe` Symb n f ns

-- Utils
newtype QuotedIdent = QuotedIdent Symb deriving (Show, Eq)
instance Arbitrary QuotedIdent where arbitrary = QuotedIdent <$> arbQuotedIdent
arbQuotedIdent = do Ident f ns <- arbitrary; return $ Symb Z f ns

newtype QuotedIdents = QuotedIdents [Symb] deriving (Show, Eq)
instance Arbitrary QuotedIdents where arbitrary = QuotedIdents <$> arbFew arbQuotedIdent

newtype QuotedSymb = QuotedSymb Symb deriving (Show, Eq)
instance Arbitrary QuotedSymb where arbitrary = QuotedSymb <$> arbQuotedSymb
arbQuotedSymb = do Ident f ns <- arbitrary; n <- arbitrary; return $ Symb (S n) f ns