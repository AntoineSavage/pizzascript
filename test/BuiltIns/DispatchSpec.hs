module BuiltIns.DispatchSpec where

import Test.Hspec
import Test.QuickCheck

import BuiltIns.Dispatch
import BuiltIns.FuncImpls as Impls
import Control.Exception
import Ops.PzValSpec

spec :: Spec
spec = describe "dispatch" $ do
    it "dispatches to boolean functions" $ do
        property $ \v1 v2 -> do
            dispatch undefined [v1] "not" `shouldBe` Right (Impls._not v1)
            dispatch undefined [v1, v2] "or" `shouldBe` Right (Impls._or v1 v2)
            dispatch undefined [v1, v2] "and" `shouldBe` Right (Impls._and v1 v2)

    it "rejects unsupported built-in function" $ do
        property $ \nameSuffix -> do
            let funcName = "$" ++ nameSuffix
            evaluate (dispatch undefined [] funcName) `shouldThrow`
                errorCall ("Built-in function '" ++ funcName ++ "' not supported")