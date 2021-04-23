module Pz.PzNumSpec where

import Test.Hspec
import Test.QuickCheck

import Pz.PzNum
import Ast.AstNum
import Ast.AstNumSpec

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
    it "converts pz integer" $ do
        property $ \n -> do
            fromAst (AstInteger n) `shouldBe` PzInteger n

    it "converts pz double" $ do
        property $ \d -> do
            fromAst (AstDouble d) `shouldBe` PzDouble d

toAstSpec :: Spec
toAstSpec = describe "toAst" $ do
    it "converts ast integer" $ do
        property $ \n -> do
            toAst (PzInteger n) `shouldBe` AstInteger n

    it "converts ast double" $ do
        property $ \d -> do
            toAst (PzDouble d) `shouldBe` AstDouble d

instance Arbitrary PzNum where
    arbitrary = fromAst <$> arbitrary      