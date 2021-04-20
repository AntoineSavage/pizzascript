module Ast.AstSymbSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Ast.AstIdent as AstIdent
import Ast.AstIdentSpec () -- instances
import Ast.AstSymb
import Control.Monad
import Data.Either
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
            let s = '\'' : AstIdent.unparse ident
            parse parser "tests" s `shouldBe` Right (AstSymb 1 ident)

    it "parses n quotes followed by ident" $ do
        property $ \(Positive n) ident -> do
            let s = replicate n '\'' ++ AstIdent.unparse ident
            parse parser "tests" s `shouldBe` Right (AstSymb n ident)

unparseSpec :: Spec
unparseSpec = describe "unparse" $ do
    it "unparses single quote followed by ident" $ do
        property $ \(Negative n) ident -> do
            unparse (AstSymb n ident) `shouldBe` '\'' : AstIdent.unparse ident
            unparse (AstSymb 0 ident) `shouldBe` '\'' : AstIdent.unparse ident
    
    it "unparses n quotes followed by ident" $ do
        property $ \(Positive n) ident -> do
            unparse (AstSymb n ident) `shouldBe` replicate n '\'' ++ AstIdent.unparse ident

instance Arbitrary AstSymb where
    arbitrary = do
        Positive n <- arbitrary
        AstSymb n <$> arbitrary