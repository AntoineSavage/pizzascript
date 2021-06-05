module Ops.Func.FuncImpureArgsSpec where

import Test.Hspec
import Test.QuickCheck

import Ops.Func.FuncImpureArgs
import Types.Func.ArgPass
import Types.Func.ArgPassSpec
import Types.Func.FuncImpureArgs
import Types.SymbSpec

spec :: Spec
spec = do
    getArgPassSpec
    getExplCtxSpec
    
getArgPassSpec :: Spec
getArgPassSpec = describe "getArgPass" $ do
    it "handles None" $ do
        getArgPass None `shouldBe` Eval

    it "handles ArgPass" $ do
        property $ \ap -> do
            getArgPass (ArgPass ap) `shouldBe` ap

    it "handles Both" $ do
        property $ \ap ->
            getArgPass (Both ap undefined) `shouldBe` ap
    
getExplCtxSpec :: Spec
getExplCtxSpec = describe "getExplCtx" $ do
    it "handles Both" $ do
        property $ \ec -> do
            getExplCtx (Both undefined ec) `shouldBe` Just ec

    it "handles ArgPass" $ do
        getExplCtx (ArgPass undefined) `shouldBe` Nothing

    it "handles None" $ do
        getExplCtx None `shouldBe` Nothing