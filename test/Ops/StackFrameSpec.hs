module Ops.StackFrameSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Ops.PzValSpec
import Ops.StackFrame
import TestUtils
import Types.PzValSpec
import Types.StackFrame
import Types.StackFrameSpec

spec :: Spec
spec = do
    setCtxSpec

setCtxSpec :: Spec
setCtxSpec = describe "setCtx" $ do
    it "returns empty list unchanged" $ do
        setCtx undefined [] `shouldBe` []

    it "sets context" $ do
        property $ \(ArbDict ctx) spec (Few fs) -> do
            setCtx ctx (StackFrame undefined spec : fs) `shouldBe` StackFrame ctx spec : fs