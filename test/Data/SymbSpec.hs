module Data.SymbSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Exception
import Control.Monad
import Data.Either
import Data.Nat
import Data.NatSpec
import Data.Symb
import TestUtils
import Text.Parsec

spec :: Spec
spec = do
    symbSpec
    parseSymbVsUnparseSymbSpec
    parseSymbSpec
    unparseSymbSpec

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

-- Utils
validFirsts = underscore : lettersUpper ++ lettersLower ++ accentChars
validNexts = underscore : digits ++ lettersUpper ++ lettersLower ++ accentChars

instance Arbitrary Symb where
    arbitrary = do
        n <- arbitrary
        Ident f ns <- arbitrary
        return $ Symb n f ns

data Ident = Ident Char String deriving (Show, Eq)
instance Arbitrary Ident where
    arbitrary = do
        first <- elements validFirsts
        nexts <- chooseInt (0, 10) >>= flip vectorOf (elements validNexts)
        return $ Ident first nexts

newtype QuotedIdent = QuotedIdent Symb deriving (Show, Eq)
instance Arbitrary QuotedIdent where arbitrary = QuotedIdent <$> arbQuotedIdent
arbQuotedIdent = do Ident f ns <- arbitrary; return $ Symb Z f ns  

newtype QuotedIdents = QuotedIdents [Symb] deriving (Show, Eq)
instance Arbitrary QuotedIdents where arbitrary = QuotedIdents <$> arbFew arbQuotedIdent