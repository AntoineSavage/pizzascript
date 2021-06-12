module ReduceSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as M

import BuiltIns.Dispatch
import BuiltIns.FuncImpls
import Control.Exception
import Eval
import Ops.Func.FuncCustom
import Ops.PzVal
import Ops.Symb
import Reduce
import TestUtils
import Types.Func
import Types.Func.FuncArgs
import Types.Func.FuncBody
import Types.Func.FuncCustom
import Types.Func.FuncImpureArgs
import Types.PzVal
import Types.PzValSpec
import Types.Symb

spec :: Spec
spec = do
    clsInvokeFuncQuotedSpec
    clsInvokeFuncEvaledSpec
    invokeFuncResultShowSpec
    invokeFuncSpec
    buildArgImplCtxSpec

clsInvokeFuncQuotedSpec :: Spec
clsInvokeFuncQuotedSpec = describe "clsInvokeFunc (Quoted instance)" $ do
    it "dispatches to quoted built-in func" $ do
        property $ \s -> do
            let funcName = '$':s
            evaluate (clsDispatch u (u :: [PzVal Quoted]) funcName) `shouldThrow` errorCall ("Built-in function '" ++ funcName ++ "' not supported (dispatchQuoted)")

    it "converts to evaled using fromQuoted" $ do
        property $ \v -> do
            clsToEvaled v `shouldBe` fromQuoted v

clsInvokeFuncEvaledSpec :: Spec
clsInvokeFuncEvaledSpec = describe "clsInvokeFunc (Evaled instance)" $ do
    it "dispatches to unquoted built-in func" $ do
        property $ \s -> do
            let funcName = '$':s
            evaluate (clsDispatch u (u :: [PzVal Evaled]) funcName) `shouldThrow` errorCall ("Built-in function '" ++ funcName ++ "' not supported (dispatch)")

    it "converts to evaled as id" $ do
        property $ \v -> do
            clsToEvaled (v :: PzVal Evaled) `shouldBe` v

invokeFuncResultShowSpec :: Spec
invokeFuncResultShowSpec = describe "InvokeFuncResult (show instance)" $ do
    it "shows ResultBuiltIn" $ do
        property $ \v -> v /= PzUnit ==> do
            show (ResultBuiltIn PzUnit) `shouldBe` "ResultBuiltIn PzUnit"
            show (ResultBuiltIn v) `shouldBe` "ResultBuiltIn (" ++ show v ++ ")"

    it "shows ResultCustom" $ do
        property $ \(ArbDict d) (Few vs) -> do
            show (ResultCustom (d, vs)) `shouldBe` "ResultCustom " ++ show (d, vs)

invokeFuncSpec :: Spec
invokeFuncSpec = describe "invokeFunc" $ do
    it "handles built-in func (quoted)" $ do
        property $ \(ArbDict d) impArgs args e es -> do
            let vs = unevalFuncCustom $ FuncCustom impArgs args e es
            invokeFunc d u u u (BodyBuiltIn $ symb "func") vs `shouldBe`
                Right (ResultBuiltIn $ PzList [PzDict d, PzFunc d $ Func impArgs args $ BodyCustom e es])

    it "handles built-in func (evaled)" $ do
        property $ \v -> do
            invokeFunc u u u u (BodyBuiltIn $ symb "type_of") [v] `shouldBe` Right (ResultBuiltIn $ _typeOf v)

    it "rejects custom func with invalid arity (quoted)" $ do
        pending

    it "rejects custom func with invalid arity (evaled)" $ do
        pending

    it "handles custom func (quoted)" $ do
        pending

    it "handles custom func (evaled)" $ do
        pending

buildArgImplCtxSpec :: Spec
buildArgImplCtxSpec = describe "buildArgImplCtx" $ do
    it "builds zero args (None)" $ do
        buildArgImplCtx u None (ArgsArity []) u `shouldBe` (0, M.empty)

    it "builds zero args (ArgPass)" $ do
        buildArgImplCtx u (ArgPass u) (ArgsArity []) u `shouldBe` (0, M.empty)

    it "builds one arg (Both)" $ do
        property $ \(ArbDict d) i -> do
            let expected = M.fromList [ (DictKey $ PzSymb i, PzDict d) ]
            buildArgImplCtx d (Both u i) (ArgsArity []) u `shouldBe` (0, expected)

    it "builds one arg (ArgsVaria)" $ do
        property $ \i (Few vs) -> do
            let expected = M.fromList [ (DictKey $ PzSymb i, PzList vs) ]
            buildArgImplCtx u None (ArgsVaria i) vs `shouldBe` (length vs, expected)

    it "builds one arg (ArgsArity)" $ do
        property $ \i (Few vs) -> do
            let expected = M.fromList $ zip (map (DictKey . PzSymb) [i]) vs
            buildArgImplCtx u None (ArgsArity [i]) vs `shouldBe` (1, expected)

    it "builds two args (Both+ArgsVaria)" $ do
        property $ \(ArbDict d) (Uniques2 i1 i2) (Few vs) -> do
            let expected = M.fromList [ (DictKey $ PzSymb i1, PzDict d), (DictKey $ PzSymb i2, PzList vs) ]
            buildArgImplCtx d (Both u i1) (ArgsVaria i2) vs `shouldBe` (length vs, expected)

    it "builds two args (Both+ArgsArity)" $ do
        property $ \(ArbDict d) (Uniques2 i1 i2) (Few vs) -> do
            let expected = M.fromList $ (DictKey $ PzSymb i1, PzDict d) : zip (map (DictKey . PzSymb) [i2]) vs
            buildArgImplCtx d (Both u i1) (ArgsArity [i2]) vs `shouldBe` (1, expected)

    it "builds two args (ArgsArity)" $ do
        property $ \(Uniques2 i1 i2) (Few vs) -> do
            let expected = M.fromList $ zip (map (DictKey . PzSymb) [i1,i2]) vs
            buildArgImplCtx u None (ArgsArity [i1,i2]) vs `shouldBe` (2, expected)

    it "builds three args (Both+ArgsArity)" $ do
        property $ \(ArbDict d) (Uniques3 i1 i2 i3) (Few vs) -> do
            let expected = M.fromList $ (DictKey $ PzSymb i1, PzDict d) : zip (map (DictKey . PzSymb) [i2,i3]) vs
            buildArgImplCtx d (Both u i1) (ArgsArity [i2,i3]) vs `shouldBe` (2, expected)

    it "builds N args (ArgsArity)" $ do
        property $ \(Uniques is) (Few vs) -> do
            let expected = M.fromList $ zip (map (DictKey . PzSymb) is) vs
            buildArgImplCtx u None (ArgsArity is) vs `shouldBe` (length is, expected)

    it "builds N+1 args (Both+ArgsArity)" $ do
        property $ \(ArbDict d) i (Uniques is) (Few vs) -> not (i `elem` is) ==> do
            let expected = M.fromList $ (DictKey $ PzSymb i, PzDict d) : zip (map (DictKey . PzSymb) is) vs
            buildArgImplCtx d (Both u i) (ArgsArity is) vs `shouldBe` (length is, expected)