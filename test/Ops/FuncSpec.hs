module Ops.FuncSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Ops.Func
import Ops.Func.FuncBodySpec
import TestUtils
import Types.Func
import Types.Func.ArgPass
import Types.Func.FuncArgsSpec
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

-- Utils
instance ArbWithDepth a => Arbitrary (Func a) where arbitrary = arbDepth
instance ArbWithDepth a => ArbWithDepth (Func a) where
    arbWithDepth depth = liftM3 Func arbitrary arbitrary $ arbWithDepth depth