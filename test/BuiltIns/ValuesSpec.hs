module BuiltIns.ValuesSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as M

import BuiltIns.Values
import BuiltIns.Pos
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
        pzFalse `shouldBe` withPos (PzSymb symbFalse)
        pzTrue `shouldBe` withPos (PzSymb symbTrue)
        pzNot `shouldBe` withPos (PzFunc M.empty $ Func None (ArgsArity builtInPos [withPos identX]) (BodyBuiltIn identNot))
        pzOr `shouldBe` withPos (PzFunc M.empty $ Func None (ArgsArity builtInPos [withPos identX, withPos identY]) (BodyBuiltIn identOr))
        pzAnd `shouldBe` withPos (PzFunc M.empty $ Func None (ArgsArity builtInPos [withPos identX, withPos identY]) (BodyBuiltIn identAnd))

    it "declares function constants" $ do
        pzFunc `shouldBe` withPos (PzFunc M.empty func)

funcSpec :: Spec
funcSpec = describe "func" $ do
    it "returns func value" $ do
        let f = func
        impArgs f `shouldBe` Both builtInPos (withPos Quote) (withPos identCtx)
        args f `shouldBe` ArgsVaria (withPos identArgs)
        body f `shouldBe` BodyBuiltIn identFunc