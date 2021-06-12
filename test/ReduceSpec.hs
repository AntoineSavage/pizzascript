module ReduceSpec where

import Test.Hspec
import Test.QuickCheck

import Eval
import Reduce
import TestUtils
import Types.PzValSpec
import Types.StackFrame
import Types.StackFrameSpec

spec :: Spec
spec = do
    toAccSpec

toAccSpec :: Spec
toAccSpec = describe "toAcc" $ do
    it "handles Evaled" $ do
        property $ \(ArbDict d) (Few fs) spec v -> do
            toAcc d fs spec (Evaled v) `shouldBe`
                Right (Acc (Just v) $ StackFrame d spec : fs)
    
    it "handles PushForm" $ do
        property $ \(ArbDict d) (Few fs) spec v vs -> do
            toAcc d fs spec (PushForm v vs) `shouldBe`
                Right (Acc Nothing $ StackFrame d (FormQuoted v vs) : StackFrame d spec : fs)