module Ast.AstSymbSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Ast.AstIdent as I

import Ast.AstIdentSpec () -- instances
import Ast.AstSymb
import Control.Monad
import Data.Nat
import Data.NatSpec
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

    it "parses one quote followed by ident" $ do
        property $ \ident -> do
            let s = '\'' : I.unparse ident
            parse parser "tests" s `shouldBe` Right (AstSymb Z ident)

    it "parses n+1 quotes followed by ident" $ do
        property $ \n ident -> do
            let s = "'" ++ unlen '\'' n ++ I.unparse ident
            parse parser "tests" s `shouldBe` Right (AstSymb n ident)

unparseSpec :: Spec
unparseSpec = describe "unparse" $ do
    it "unparses one quote followed by ident" $ do
        property $ \ident -> do
            unparse (AstSymb Z ident) `shouldBe` '\'' : I.unparse ident
    
    it "unparses n+1 quotes followed by ident" $ do
        property $ \n ident -> do
            unparse (AstSymb n ident) `shouldBe` "'" ++ unlen '\'' n ++ I.unparse ident

instance Arbitrary AstSymb where
    arbitrary = liftM2 AstSymb arbitrary arbitrary