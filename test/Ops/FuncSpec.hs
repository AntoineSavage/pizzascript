module Ops.FuncSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Ops.Func
import TestUtils
import Types.Func
import Types.FuncSpec
import Types.Func.ArgPass
import Types.Func.FuncArgsSpec
import Types.Func.FuncBodySpec
import Types.Func.FuncImpureArgs
import Types.Func.FuncImpureArgsSpec

spec :: Spec
spec = do
    getArgPassSpec

getArgPassSpec :: Spec
getArgPassSpec = describe "getArgPass" $ do
    it "converts None to Eval" $ do
        property $ \args body ->
            getArgPass (Func None args body :: Func Int) `shouldBe` Eval

    it "converts ArgPass" $ do
        property $ \ap args body ->
            getArgPass (Func (ArgPass ap) args body :: Func Int) `shouldBe` ap

    it "converts Both" $ do
        property $ \ap explCtx args body ->
            getArgPass (Func (Both ap explCtx) args body :: Func Int) `shouldBe` ap