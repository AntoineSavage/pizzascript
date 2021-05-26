module BuiltIns.CtxSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as M

import BuiltIns.Ctx
import BuiltIns.FuncValues
import Symbs
import Types.PzVal

spec :: Spec
spec = do
    builtInCtxSpec

builtInCtxSpec :: Spec
builtInCtxSpec = describe "builtInCtx" $ do
    it "contains the required keys" $ do
        let keys = [pzSymbFalse, pzSymbTrue, pzSymbNot, pzSymbOr, pzSymbAnd, pzSymbFunc]
            values = [pzSymbFalse, pzSymbTrue, pzNot, pzOr, pzAnd, pzFunc]
        builtInCtx `shouldBe` M.fromList (zip keys values)