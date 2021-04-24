module Pz.PzNumSpec where

import Test.Hspec
import Test.QuickCheck

import Pz.PzNum
import Ast.AstNum
import Ast.AstNumSpec

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
    it "converts pz integer" $ do
        property $ \n -> do
            eval (AstInteger n) `shouldBe` PzInteger n

    it "converts pz double" $ do
        property $ \d -> do
            eval (AstDouble d) `shouldBe` PzDouble d

unevalSpec :: Spec
unevalSpec = describe "uneval" $ do
    it "converts ast integer" $ do
        property $ \n -> do
            uneval (PzInteger n) `shouldBe` AstInteger n

    it "converts ast double" $ do
        property $ \d -> do
            uneval (PzDouble d) `shouldBe` AstDouble d

instance Arbitrary PzNum where
    arbitrary = eval <$> arbitrary      