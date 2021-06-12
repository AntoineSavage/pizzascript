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
    reduceSpec
    reduceBlockSpec
    reduceFormQuotedSpec
    reduceFormEvaledSpec
    reduceInvocArgsSpec
    reduceInvocEvaledSpec
    reduceInvocSpec
    toAccSpec

reduceSpec :: Spec
reduceSpec = describe "reduce" $ do
    it "handles empty stack frames" $ do
        pending

    it "reduces block" $ do
        pending

    it "reduces form (quoted)" $ do
        pending

    it "reduces form (evaled)" $ do
        pending

    it "reduces invoc (quoted)" $ do
        pending

    it "reduces invoc (args)" $ do
        pending

    it "reduces invoc (evaled)" $ do
        pending

reduceBlockSpec :: Spec
reduceBlockSpec = describe "reduceBlock" $ do
    it "handles empty block" $ do
        pending

    it "evals next block element" $ do
        pending

reduceFormQuotedSpec :: Spec
reduceFormQuotedSpec = describe "reduceFormQuoted" $ do
    it "evals first form element" $ do
        pending

    it "handles evaled first form element" $ do
        pending

reduceFormEvaledSpec :: Spec
reduceFormEvaledSpec = describe "reduceFormEvaled" $ do
    it "rejects malformed function invocation" $ do
        pending

    it "handles eval function invocation" $ do
        pending

    it "handles quote function invocation" $ do
        pending

    it "rejects unquote function invocation" $ do
        pending

    it "rejects deep-quote function invocation" $ do
        pending

    it "rejects deep-unquote function invocation" $ do
        pending

reduceInvocArgsSpec :: Spec
reduceInvocArgsSpec = describe "reduceInvocArgs" $ do
    it "handles single evaluated arg" $ do
        pending

    it "handles all evaluated args" $ do
        pending

    it "evaluates single arg" $ do
        pending

reduceInvocEvaledSpec :: Spec
reduceInvocEvaledSpec = describe "reduceInvocEvaled" $ do
    it "invokes function" $ do
        pending

    it "handles pure function output" $ do
        pending

    it "handles impure valid function output" $ do
        pending

    it "rejects impure invalid function output" $ do
        pending

reduceInvocSpec :: Spec
reduceInvocSpec = describe "reduceInvoc" $ do
    it "handles built-in function invocation" $ do
        pending

    it "handles custom function invocation" $ do
        pending

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