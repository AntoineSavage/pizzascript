module Data.PzSymbSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Either
import qualified Data.PzIdent as I
import Data.PzIdentSpec ()
import Data.PzSymb
import Text.Parsec

spec :: Spec
spec = do
    parseVsUnparseSpec
    parseSpec
    unparseSpec

parseVsUnparseSpec :: Spec
parseVsUnparseSpec = describe "parse vs unparse" $ do
    it "composes parse and unparse" $ do
        property $ \symb -> do
            let s = unparse symb
            parse parser "tests" s `shouldBe` Right symb
            unparse <$> parse parser "tests" s `shouldBe` Right (unparse symb)

parseSpec :: Spec
parseSpec = describe "parse" $ do
    it "rejects empty string" $ do
        isLeft (parse parser "tests" "") `shouldBe` True

    it "parses single quote followed by ident" $ do
        property $ \ident -> do
            let s = '\'' : I.unparse ident
            parse parser "tests" s `shouldBe` Right (PzSymb 1 ident)

    it "parses n quotes followed by ident" $ do
        property $ \(Positive n) ident -> do
            let s = replicate n '\'' ++ I.unparse ident
            parse parser "tests" s `shouldBe` Right (PzSymb n ident)

unparseSpec :: Spec
unparseSpec = describe "unparse" $ do
    it "unparses single quote followed by ident" $ do
        property $ \(Negative n) ident -> do
            unparse (PzSymb n ident) `shouldBe` '\'' : I.unparse ident
            unparse (PzSymb 0 ident) `shouldBe` '\'' : I.unparse ident
    
    it "unparses n quotes followed by ident" $ do
        property $ \(Positive n) ident -> do
            unparse (PzSymb n ident) `shouldBe` replicate n '\'' ++ I.unparse ident

instance Arbitrary PzSymb where
    arbitrary = do
        Positive n <- arbitrary
        PzSymb n <$> arbitrary