module BuiltInsSpec where

import Test.Hspec
import Test.QuickCheck

import BuiltIns
import TestUtils
import Text.Parsec.Pos
import Types

spec :: Spec
spec = do
    toFormSpec

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