module Data.FuncCustomSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as M

import Data.FuncArgs
import Data.FuncImpureArgs
import Data.FuncBody
import Data.FuncCustom
import Data.List
import TestUtils
import TestUtils2
import Types
import Utils.ArbWithDepth

spec :: Spec
spec = do
    toFuncCustomVsFromFuncCustomSpec
    toFuncCustomSpec
    fromFuncCustomSpec

toFuncCustomVsFromFuncCustomSpec :: Spec
toFuncCustomVsFromFuncCustomSpec = describe "toFuncCustom vs fromFuncCustom" $ do
    it "composes toFuncCustom and fromFuncCustom into id" $ do
        property $ \(ArbDict ctx) funcCustom -> do
            let func = fromFuncCustom ctx funcCustom
            toFuncCustom func `shouldBe` Right funcCustom
            fromFuncCustom ctx <$> toFuncCustom func `shouldBe` Right func

toFuncCustomSpec :: Spec
toFuncCustomSpec = describe "toFuncCustom" $ do
    it "rejects built-in function" $ do
        property $ \impArgs args ident ->
            toFuncCustom (Func undefined impArgs args $ BodyBuiltIn ident) `shouldBe` Left ident

    it "converts custom function" $ do
        property $ \impArgs args es ->
            toFuncCustom (Func undefined impArgs args $ BodyCustom es) `shouldBe` Right (FuncCustom impArgs args es)

fromFuncCustomSpec :: Spec
fromFuncCustomSpec = describe "fromFuncCustom" $ do
    it "converts to function (smallest)" $ do
        property $ \p -> do
            fromFuncCustom M.empty (FuncCustom None (ArgsArity p []) []) `shouldBe` Func M.empty None (ArgsArity p []) (BodyCustom [])

    it "converts to function (prop)" $ do
        property $ \(ArbDict implCtx) impArgs args (Few es) -> do
            fromFuncCustom implCtx (FuncCustom impArgs args es) `shouldBe` Func implCtx impArgs args (BodyCustom es)

-- Utils
instance Arbitrary FuncCustom where arbitrary = arbDepth
instance ArbWithDepth FuncCustom where
    arbWithDepth depth = do
        let getImpArgsIdents (Both _ _ i) = [i]
            getImpArgsIdents _            = []

            getArgsIdents (ArgsVaria i)    = [i]
            getArgsIdents (ArgsArity _ is) = is

        impArgs <- arbitrary
        args <- arbitrary
        let is = getImpArgsIdents impArgs ++ getArgsIdents args
        if is == nub is
            then fmap (FuncCustom impArgs args) $ arbWithDepth depth
            else arbWithDepth depth