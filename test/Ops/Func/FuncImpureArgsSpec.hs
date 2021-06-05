module Ops.Func.FuncImpureArgsSpec where

import Test.Hspec
import Test.QuickCheck

import Ops.Func.FuncImpureArgs
import Types.Func.ArgPass
import Types.Func.ArgPassSpec
import Types.Func.FuncImpureArgs

spec :: Spec
spec = describe "getArgPass" $ do
    it "handles None" $ do
        getArgPass None `shouldBe` Eval

    it "hanldes ArgPass" $ do
        property $ \ap -> do
            getArgPass (ArgPass ap) `shouldBe` ap

    it "handles Both" $ do
        property $ \ap ->
            getArgPass (Both ap undefined) `shouldBe` ap