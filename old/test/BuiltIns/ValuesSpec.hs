module BuiltIns.ValuesSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as M

import BuiltIns.Values
import Data.Func
import Data.Func.ArgPass
import Data.Func.FuncArgs
import Data.Func.FuncBody
import Data.Func.FuncImpureArgs
import Data.PzVal
import Idents
import Symbs

spec :: Spec
spec = do
    constantsSpec
    funcSpec

constantsSpec :: Spec
constantsSpec = describe "constants" $ do
    it "declares boolean constants" $ do
        pzFalse `shouldBe` (PzSymb symbFalse)
        pzTrue `shouldBe` (PzSymb symbTrue)
        pzNot `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [identX]) (BodyBuiltIn identNot))
        pzOr `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [identX, identY]) (BodyBuiltIn identOr))
        pzAnd `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [identX, identY]) (BodyBuiltIn identAnd))

    it "declares function constants" $ do
        pzFunc `shouldBe` (PzFunc M.empty func)

funcSpec :: Spec
funcSpec = describe "func" $ do
    it "returns func value" $ do
        let f = func
        impArgs f `shouldBe` Both (Quote) (identCtx)
        args f `shouldBe` ArgsVaria (identArgs)
        body f `shouldBe` BodyBuiltIn identFunc