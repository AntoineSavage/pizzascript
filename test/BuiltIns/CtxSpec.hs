module BuiltIns.CtxSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as M

import BuiltIns.Ctx
import BuiltIns.Values
import Symbs
import Types.PzVal

spec :: Spec
spec = do
    builtInCtxSpec

builtInCtxSpec :: Spec
builtInCtxSpec = describe "builtInCtx" $ do
    it "contains the required keys" $ do
        let keySymbs = [symbFalse, symbTrue, symbNot, symbOr, symbAnd, symbFunc]
            keys = map PzSymb keySymbs
            values = [pzFalse, pzTrue, pzNot, pzOr, pzAnd, pzFunc]
        builtInCtx `shouldBe` M.fromList (zip keys values)