module Ops.Func.FuncCustomSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.List
import Ops.Func.FuncCustom
import Ops.PzValSpec
import TestUtils
import Types.Func
import Types.Func.FuncArgs
import Types.Func.FuncBody
import Types.Func.FuncCustom
import Types.Func.FuncCustomSpec
import Types.Func.FuncImpureArgs

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