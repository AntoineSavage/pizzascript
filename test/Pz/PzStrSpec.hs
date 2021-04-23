module Pz.PzStrSpec where

import Test.Hspec
import Test.QuickCheck

import Pz.PzStr
import Ast.AstStr
import Ast.AstStrSpec

spec :: Spec
spec = do
    fromAstVsToAstSpec
    fromAstSpec
    toAstSpec

fromAstVsToAstSpec :: Spec
fromAstVsToAstSpec = describe "fromAst vs toAst" $ do
    it "composes fromAst and toAst into id" $ do
        property $ \ast pz -> do
            toAst (fromAst ast) `shouldBe` ast
            fromAst (toAst pz) `shouldBe` pz

fromAstSpec :: Spec
fromAstSpec = describe "fromAst" $ do
    it "converts pz string" $ do
        property $ \s -> do
            fromAst (AstStr s) `shouldBe` PzStr s

toAstSpec :: Spec
toAstSpec = describe "toAst" $ do
    it "converts ast string" $ do
        property $ \s -> do
            toAst (PzStr s) `shouldBe` AstStr s

instance Arbitrary PzStr where
    arbitrary = PzStr <$> (chooseInt (0, 20) >>= vector)