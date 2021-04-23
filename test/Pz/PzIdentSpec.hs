module Pz.PzIdentSpec where

import Test.Hspec
import Test.QuickCheck

import Ast.AstIdent
import Ast.AstIdentSpec
import Control.Monad
import Pz.PzIdent

spec :: Spec
spec = do
    fromAstVsToAstSpec
    fromAstSpec
    toAstSpec
    fromAstIdentVsToAstIdentSpec
    fromAstIdentSpec
    toAstIdentSpec

fromAstVsToAstSpec :: Spec
fromAstVsToAstSpec = describe "fromAst vs toAst" $ do
    it "composes fromAst and toAst into id" $ do
        property $ \ast pz -> do
            toAst (fromAst ast) `shouldBe` ast
            fromAst (toAst pz) `shouldBe` pz

fromAstSpec :: Spec
fromAstSpec = describe "fromAst" $ do
    it "converts pz string" $ do
        property $ \p ps -> do
            fromAst (AstIdent p ps) `shouldBe` PzIdent (fromAstIdent p) (map fromAstIdent ps)

toAstSpec :: Spec
toAstSpec = describe "toAst" $ do
    it "converts ast string" $ do
        property $ \p ps -> do
            toAst (PzIdent (fromAstIdent p) $ map fromAstIdent ps) `shouldBe` AstIdent p ps


fromAstIdentVsToAstIdentSpec :: Spec
fromAstIdentVsToAstIdentSpec = describe "fromAstIdent vs toAstIdent" $ do
    it "composes fromAstIdent and toAstIdent into id" $ do
        property $ \ast pz -> do
            toAstIdent (fromAstIdent ast) `shouldBe` ast
            fromAstIdent (toAstIdent pz) `shouldBe` pz

fromAstIdentSpec :: Spec
fromAstIdentSpec = describe "fromAstIdent" $ do
    it "converts pz string" $ do
        property $ \c s -> do
            fromAstIdent (AstIdentPart c s) `shouldBe` PzIdentPart c s

toAstIdentSpec :: Spec
toAstIdentSpec = describe "toAstIdent" $ do
    it "converts ast string" $ do
        property $ \c s -> do
            toAstIdent (PzIdentPart c s) `shouldBe` AstIdentPart c s

instance Arbitrary PzIdent where
    arbitrary = fromAst <$> arbitrary

instance Arbitrary PzIdentPart where
    arbitrary = fromAstIdent <$> arbitrary