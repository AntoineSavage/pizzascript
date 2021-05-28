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

    it "sets context on block frame" $ do
        property $ \(ArbDict ctx) (Few es) (Few fs) -> do
            setCtx ctx (StackFrame undefined (Block es) :fs) `shouldBe` StackFrame ctx (Block es) :fs

    it "sets context on form frame" $ do
        property $ \(ArbDict ctx) mfs e (Few es) (Few fs) -> do
            setCtx ctx (StackFrame undefined (Form mfs e es) :fs) `shouldBe` StackFrame ctx (Form mfs e es) :fs

    it "sets context on invoc frame " $ do
        property $ \(ArbDict ctx) mfs f (Few as) (Few es) (Few fs) -> do
            setCtx ctx (StackFrame undefined (Invoc mfs ctx f as $ Just es) :fs) `shouldBe` StackFrame ctx (Invoc mfs ctx f as $ Just es) :fs
            setCtx ctx (StackFrame undefined (Invoc mfs ctx f as Nothing) :fs) `shouldBe` StackFrame ctx (Invoc mfs ctx f as Nothing) :fs