module BuiltIns.CtxSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as M

import BuiltIns.Ctx
import BuiltIns.Pos
import BuiltIns.Values
import Data.Boolish
import Data.BoolishSpec
import Data.PzVal
import Data.Symb
import Idents

spec :: Spec
spec = do
    builtInCtxSpec

builtInCtxSpec :: Spec
builtInCtxSpec = describe "builtInCtx" $ do
    it "contains the required keys" $ do
        let keyIdents = [identFalse, identTrue, identNot, identOr, identAnd, identFunc]
            keys = flip map keyIdents $ withPos . PzSymb . symb
            values = [pzFalse, pzTrue, pzNot, pzOr, pzAnd, pzFunc]
        builtInCtx `shouldBe` M.fromList (zip keys values)