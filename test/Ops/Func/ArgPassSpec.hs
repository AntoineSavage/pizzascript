module Ops.Func.ArgPassSpec where

import Test.Hspec
import Test.QuickCheck

import Ops.Func.ArgPass
import Ops.Symb
import Symbs
import TestUtils
import Types.Func.ArgPass

spec :: Spec
spec = do
    argPassToSymbVsSymbToArgPassSpec
    argPassToSymbSpec
    symbToArgPassSpec

argPassToSymbVsSymbToArgPassSpec :: Spec
argPassToSymbVsSymbToArgPassSpec = describe "argPassToSymb vs symbToArgPass" $ do
    it "composes argPassToSymb and symbToArgPass into id" $ do
        property $ \argPass -> do
            let symb = argPassToSymb argPass
            symbToArgPass symb `shouldBe` Just argPass
            argPassToSymb <$> symbToArgPass symb `shouldBe` Just symb

argPassToSymbSpec :: Spec
argPassToSymbSpec = describe "argPassToSymb" $ do
    it "converts known arg passes" $ do
        argPassToSymb Eval `shouldBe` symbEval
        argPassToSymb Quote `shouldBe` symbQuote
        argPassToSymb Unquote `shouldBe` symbUnquote
        argPassToSymb DeepQuote `shouldBe` symbDeepQuote
        argPassToSymb DeepUnquote `shouldBe` symbDeepUnquote

symbToArgPassSpec :: Spec
symbToArgPassSpec = describe "symbToArgPass" $ do
    it "converts known arg pass symbols" $ do
        symbToArgPass symbEval `shouldBe` Just Eval
        symbToArgPass symbQuote `shouldBe` Just Quote
        symbToArgPass symbUnquote `shouldBe` Just Unquote
        symbToArgPass symbDeepQuote `shouldBe` Just DeepQuote
        symbToArgPass symbDeepUnquote `shouldBe` Just DeepUnquote

    it "rejects unknown symbols" $ do
        symbToArgPass (symb "ABC") `shouldBe` Nothing

    it "rejects unknown symbols (prop)" $ do
        property $ \s ->
            symbToArgPass (symb $ "_" ++ s) `shouldBe` Nothing

-- Utils
argPasses = [ Eval, Quote, Unquote, DeepQuote, DeepUnquote ]

instance Arbitrary ArgPass where arbitrary = elements argPasses
instance ArbWithDepth ArgPass where arbWithDepth _ = arbitrary