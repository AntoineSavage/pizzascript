module Data.StackFrameSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.PzValSpec
import Data.StackFrame
import TestUtils

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

-- Utils
instance Arbitrary StackFrame where arbitrary = arbDepth
instance ArbWithDepth StackFrame where arbWithDepth depth = liftM2 StackFrame arbDepth arbDepth

instance Arbitrary StackFrameSpec where arbitrary = arbDepth
instance ArbWithDepth StackFrameSpec where
    arbWithDepth depth = oneof
        [ fmap Block arbDepth
        , liftM3 Form arbitrary arbDepth arbDepth
        , do
            a <- arbitrary
            b <- arbDepth
            c <- arbDepth
            d <- arbDepth
            e <- arbDepth
            return $ Invoc a b c d e
        ]