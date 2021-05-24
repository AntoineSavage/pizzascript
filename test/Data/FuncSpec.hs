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