module Data.FuncSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.ArgPass
import Data.Func
import Data.FuncArgsSpec
import Data.FuncBodySpec
import Data.FuncImpureArgs
import Data.FuncImpureArgsSpec
import Data.WithPos
import Data.WithPosSpec
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
        property $ \p ap args body ->
            getArgPass (Func (ArgPass p $ WithPos p ap) args body) `shouldBe` ap

    it "converts Both" $ do
        property $ \p ap explCtx args body ->
            getArgPass (Func (Both p (WithPos p ap) explCtx) args body) `shouldBe` ap

-- Utils
instance Arbitrary Func where arbitrary = arbDepth
instance ArbWithDepth Func where arbWithDepth depth = liftM3 Func arbitrary arbitrary (arbWithDepth depth)