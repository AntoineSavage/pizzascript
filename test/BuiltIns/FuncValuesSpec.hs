module BuiltIns.FuncValuesSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as M

import BuiltIns.FuncValues
import Symbs
import Types.Func
import Types.Func.ArgPass
import Types.Func.FuncArgs
import Types.Func.FuncBody
import Types.Func.FuncImpureArgs
import Types.PzVal

spec :: Spec
spec = do
    it "declares generic func values" $ do
        pzTypeOf `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX]) (BodyBuiltIn symbTypeOf))
        pzEq `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX, symbY]) (BodyBuiltIn symbEq))
        pzLt `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX, symbY]) (BodyBuiltIn symbLt))

    it "declares semi-generic func values" $ do
        pzIsEmpty `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX]) (BodyBuiltIn symbIsEmpty))
        pzSize `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX]) (BodyBuiltIn symbSize))

    it "declares number func values" $ do
        pzNum `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX]) (BodyBuiltIn symbNum))
        pzAdd `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX, symbY]) (BodyBuiltIn symbAdd))
        pzSub `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX, symbY]) (BodyBuiltIn symbSub))
        pzMult `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX, symbY]) (BodyBuiltIn symbMult))
        pzDiv `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX, symbY]) (BodyBuiltIn symbDiv))
        pzRem `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX, symbY]) (BodyBuiltIn symbRem))
        pzExp `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX, symbY]) (BodyBuiltIn symbExp))
        pzLog `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX, symbY]) (BodyBuiltIn symbLog))
        pzRound `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX]) (BodyBuiltIn symbRound))
        pzFloor `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX]) (BodyBuiltIn symbFloor))
        pzCeil `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX]) (BodyBuiltIn symbCeil))
        pzTrunc `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX]) (BodyBuiltIn symbTrunc))

    it "declares string func values" $ do
        pzStr `shouldBe` (PzFunc M.empty $ Func None (ArgsVaria symbArgs) (BodyBuiltIn symbStr))
        pzSplit `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX, symbY]) (BodyBuiltIn symbSplit))
        pzJoin `shouldBe` (PzFunc M.empty $ Func None (ArgsVaria symbArgs) (BodyBuiltIn symbJoin))

    it "declares symbol func values" $ do
        pzSymb `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX]) (BodyBuiltIn symbSymb))
        pzNbrQuotes `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX]) (BodyBuiltIn symbNbrQuotes))

    it "declares boolean func values" $ do
        pzNot `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX]) (BodyBuiltIn symbNot))
        pzOr `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX, symbY]) (BodyBuiltIn symbOr))
        pzAnd `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX, symbY]) (BodyBuiltIn symbAnd))

    it "declares list func values" $ do
        pzCons `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX, symbY]) (BodyBuiltIn symbCons))
        pzHead `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX]) (BodyBuiltIn symbHead))
        pzTail `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX]) (BodyBuiltIn symbTail))

    it "declares dictionary func values" $ do
        pzKeys `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX]) (BodyBuiltIn symbKeys))
        pzAssocs `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX]) (BodyBuiltIn symbAssocs))
        pzContains `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX, symbY]) (BodyBuiltIn symbContains))
        pzGet `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX, symbY]) (BodyBuiltIn symbGet))
        pzPut `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX, symbY, symbZ]) (BodyBuiltIn symbPut))
        pzDel `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX, symbY]) (BodyBuiltIn symbDel))

    it "declares function func values" $ do
        pzFunc `shouldBe` (PzFunc M.empty $ Func (Both Quote symbCtx) (ArgsVaria symbArgs) $ BodyBuiltIn symbFunc)
        pzGetImplCtx `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX]) (BodyBuiltIn symbGetImplCtx))
        pzSetImplCtx `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX, symbY]) (BodyBuiltIn symbSetImplCtx))
        pzGetExplCtx `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX]) (BodyBuiltIn symbGetExplCtx))
        pzGetArgPass `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX]) (BodyBuiltIn symbGetArgPass))
        pzGetArgs `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX]) (BodyBuiltIn symbGetArgs))
        pzGetBody `shouldBe` (PzFunc M.empty $ Func None (ArgsArity [symbX]) (BodyBuiltIn symbGetBody))