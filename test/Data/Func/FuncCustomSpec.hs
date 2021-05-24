module Data.Func.FuncCustomSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as M

import Control.Monad
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
        property $ \impArgs args e (Few es) ->
            toFuncCustom (Func impArgs args $ BodyCustom e es) `shouldBe` Right (FuncCustom impArgs args e es)

fromFuncCustomSpec :: Spec
fromFuncCustomSpec = describe "fromFuncCustom" $ do
    it "converts to function (smallest)" $ do
        property $ \e -> do
            fromFuncCustom (FuncCustom None (ArgsArity []) e []) `shouldBe` Func None (ArgsArity []) (BodyCustom e [])

    it "converts to function (prop)" $ do
        property $ \impArgs args e (Few es) -> do
            fromFuncCustom (FuncCustom impArgs args e es) `shouldBe` Func impArgs args (BodyCustom e es)

-- Utils
instance Arbitrary FuncCustom where arbitrary = arbDepth
instance ArbWithDepth FuncCustom where
    arbWithDepth depth = do
        let getImpArgsIdents (Both _ s) = [s]
            getImpArgsIdents _          = []

            getArgsIdents (ArgsVaria s)  = [s]
            getArgsIdents (ArgsArity ss) = ss

        impArgs <- arbitrary
        args <- arbitrary
        let ss = getImpArgsIdents impArgs ++ getArgsIdents args
        if ss == nub ss
            then liftM2 (FuncCustom impArgs args) (arbWithDepth depth) $ arbWithDepth depth
            else arbWithDepth depth