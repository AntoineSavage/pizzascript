module Pz.PzStrSpec where

import Test.Hspec
import Test.QuickCheck

import Pz.PzStr
import Ast.AstStr
import Ast.AstStrSpec

spec :: Spec
spec = do
    evalVsUnevalSpec
    evalSpec
    unevalSpec

evalVsUnevalSpec :: Spec
evalVsUnevalSpec = describe "eval vs uneval" $ do
    it "composes eval and uneval into id" $ do
        property $ \ast pz -> do
            uneval (eval ast) `shouldBe` ast
            eval (uneval pz) `shouldBe` pz

evalSpec :: Spec
evalSpec = describe "eval" $ do
    it "converts pz string" $ do
        property $ \s -> do
            eval (AstStr s) `shouldBe` PzStr s

unevalSpec :: Spec
unevalSpec = describe "uneval" $ do
    it "converts ast string" $ do
        property $ \s -> do
            uneval (PzStr s) `shouldBe` AstStr s

instance Arbitrary PzStr where
    arbitrary = eval <$> arbitrary