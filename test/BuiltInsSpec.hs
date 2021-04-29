module BuiltInsSpec where

import Test.Hspec
import Test.QuickCheck

import BuiltIns
import TestUtils
import Text.Parsec.Pos
import Types

spec :: Spec
spec = do
    toSymbVsFromSymbSpec
    toSymbSpec
    fromSymbSpec
    toFormSpec

toSymbVsFromSymbSpec :: Spec
toSymbVsFromSymbSpec = describe "toSymb vs fromSymb" $ do
    it "composes toSymb and fromSymb into id" $ do
        property $ \(ArgPassSymb s) -> do
            toSymb <$> fromSymb s `shouldBe` Just s
            fromSymb <$> toSymb <$> fromSymb s `shouldBe` fromSymb <$> Just s

toSymbSpec :: Spec
toSymbSpec = describe "toSymb" $ do
    it "converts supported arg pass values" $ do
        toSymb Eval `shouldBe` symbEval
        toSymb Quote `shouldBe` symbQuote
        toSymb Unquote `shouldBe` symbUnquote
        toSymb DeepQuote `shouldBe` symbDeepQuote
        toSymb DeepUnquote `shouldBe` symbDeepUnquote

fromSymbSpec :: Spec
fromSymbSpec = describe "fromSymb" $ do
    it "fails for unsupported symbols" $ do
        property $ \s -> do
            fromSymb (symb $ "_" ++ s) `shouldBe` Nothing

    it "succeeds for supported symbols" $ do
        fromSymb symbEval `shouldBe` Just Eval
        fromSymb symbQuote `shouldBe` Just Quote
        fromSymb symbUnquote `shouldBe` Just Unquote
        fromSymb symbDeepQuote `shouldBe` Just DeepQuote
        fromSymb symbDeepUnquote `shouldBe` Just DeepUnquote

toFormSpec :: Spec
toFormSpec = describe "toForm" $ do
    it "converts empty list" $ do
        toForm pos KindList [] `shouldBe` [AstExpr pos "" $ AstIdent $ identList]
        toForm pos KindDict [] `shouldBe` [AstExpr pos "" $ AstIdent $ identDict]
        toForm pos KindForm [] `shouldBe` []

    it "converts list" $ do
        property $ \es -> do
            toForm pos KindList es `shouldBe` (AstExpr pos "" $ AstIdent $ identList) : es
            toForm pos KindDict es `shouldBe` (AstExpr pos "" $ AstIdent $ identDict) : es
            toForm pos KindForm es `shouldBe` es