module Data.FuncSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Func
import Data.Func.ArgPass
import Data.Func.FuncArgsSpec
import Data.Func.FuncBodySpec
import Data.Func.FuncImpureArgs
import Data.Func.FuncImpureArgsSpec
import TestUtils

spec :: Spec
spec = do
    getArgPassSpec

getArgPassSpec :: Spec
getArgPassSpec = describe "getArgPass" $ do
    it "converts None to Eval" $ do
        property $ \args body ->
            getArgPass (Func None args body) `shouldBe` Eval

    it "converts ArgPass" $ do
        property $ \ap args body ->
            getArgPass (Func (ArgPass ap) args body) `shouldBe` ap

    it "converts Both" $ do
        property $ \ap explCtx args body ->
            getArgPass (Func (Both ap explCtx) args body) `shouldBe` ap

-- Utils
instance Arbitrary Func where arbitrary = arbDepth
instance ArbWithDepth Func where arbWithDepth depth = liftM3 Func arbitrary arbitrary (arbWithDepth depth)