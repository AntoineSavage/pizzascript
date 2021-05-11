module Data.SymbSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Ident
import Data.IdentSpec
import Data.Either
import Data.Nat
import Data.NatSpec
import Data.Symb
import Text.Parsec

spec :: Spec
spec = do
    parseSymbVsUnparseSymbSpec
    parseSymbSpec
    unparseSymbSpec

parseSymbVsUnparseSymbSpec :: Spec
parseSymbVsUnparseSymbSpec = describe "parseSymb vs unparseSymb" $ do
    it "composes parseSymb and unparseSymb into id" $ do
        property $ \n ident -> do
            let s = "'" ++ unlen n '\'' ++ unparseIdent ident
            parse parseSymb "tests" s `shouldBe` Right (Symb n ident)
            unparseSymb <$> parse parseSymb "tests" s `shouldBe` Right s

parseSymbSpec :: Spec
parseSymbSpec = describe "parseSymb" $ do
    it "rejects empty string" $ do
        isLeft (parse parseSymb "tests" "") `shouldBe` True

    it "parses one quote followed by ident" $ do
        property $ \ident -> do
            let s = '\'' : unparseIdent ident
            parse parseSymb "tests" s `shouldBe` Right (Symb Z ident)

    it "parses n+1 quotes followed by ident" $ do
        property $ \n ident -> do
            let s = "'" ++ unlen n '\'' ++ unparseIdent ident
            parse parseSymb "tests" s `shouldBe` Right (Symb n ident)

unparseSymbSpec :: Spec
unparseSymbSpec = describe "unparseSymb" $ do
    it "unparses one quote followed by ident" $ do
        property $ \ident -> do
            unparseSymb (Symb Z ident) `shouldBe` "'" ++ unparseIdent ident
   
    it "unparses n+1 quotes followed by ident" $ do
        property $ \n ident -> do
            unparseSymb (Symb n ident) `shouldBe` "'" ++ unlen n '\'' ++ unparseIdent ident

-- Utils