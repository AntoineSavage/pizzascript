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
        pzNot `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX]) (BodyBuiltIn symbNot))
        pzOr `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX, symbY]) (BodyBuiltIn symbOr))
        pzAnd `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX, symbY]) (BodyBuiltIn symbAnd))

    it "declares function constants" $ do
        pzFunc `shouldBe` (PzFunc M.empty func)

funcSpec :: Spec
funcSpec = describe "func" $ do
    it "returns func value" $ do
        let f = func :: Func ()
        impArgs f `shouldBe` Both (Quote) (symbCtx)
        args f `shouldBe` ArgsVaria (symbArgs)
        body f `shouldBe` BodyBuiltIn symbFunc