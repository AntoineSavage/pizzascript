module Data.Func.FuncCustomSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as M

import Data.Func
import Data.Func.FuncArgs
import Data.Func.FuncArgsSpec
import Data.Func.FuncImpureArgs
import Data.Func.FuncImpureArgsSpec
import Data.Func.FuncBody
import Data.Func.FuncCustom
import Data.PzValSpec
import Data.List
import TestUtils

spec :: Spec
spec = do
    toFuncCustomVsFromFuncCustomSpec
    toFuncCustomSpec
    fromFuncCustomSpec

toFuncCustomVsFromFuncCustomSpec :: Spec
toFuncCustomVsFromFuncCustomSpec = describe "toFuncCustom vs fromFuncCustom" $ do
    it "composes toFuncCustom and fromFuncCustom into id" $ do
        property $ \funcCustom -> do
            let func = fromFuncCustom funcCustom
            toFuncCustom func `shouldBe` Right funcCustom
            fromFuncCustom <$> toFuncCustom func `shouldBe` Right func

toFuncCustomSpec :: Spec
toFuncCustomSpec = describe "toFuncCustom" $ do
    it "rejects built-in function" $ do
        property $ \impArgs args ident ->
            toFuncCustom (Func impArgs args $ BodyBuiltIn ident) `shouldBe` Left ident

    it "converts custom function" $ do
        property $ \impArgs args es ->
            toFuncCustom (Func impArgs args $ BodyCustom es) `shouldBe` Right (FuncCustom impArgs args es)

fromFuncCustomSpec :: Spec
fromFuncCustomSpec = describe "fromFuncCustom" $ do
    it "converts to function (smallest)" $ do
        fromFuncCustom (FuncCustom None (ArgsArity []) []) `shouldBe` Func None (ArgsArity []) (BodyCustom [])

    it "converts to function (prop)" $ do
        property $ \impArgs args (Few es) -> do
            fromFuncCustom (FuncCustom impArgs args es) `shouldBe` Func impArgs args (BodyCustom es)

-- Utils
instance Arbitrary FuncCustom where arbitrary = arbDepth
instance ArbWithDepth FuncCustom where
    arbWithDepth depth = do
        let getImpArgsIdents (Both _ i) = [i]
            getImpArgsIdents _            = []

            getArgsIdents (ArgsVaria i)    = [i]
            getArgsIdents (ArgsArity is) = is

        impArgs <- arbitrary
        args <- arbitrary
        let is = getImpArgsIdents impArgs ++ getArgsIdents args
        if is == nub is
            then fmap (FuncCustom impArgs args) $ arbWithDepth depth
            else arbWithDepth depth