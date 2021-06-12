module ReduceSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as M

import BuiltIns.Dispatch
import BuiltIns.FuncImpls
import Control.Exception
import Data.List
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
    reductionShowSpec
    clsInvokeFuncQuotedSpec
    clsInvokeFuncEvaledSpec
    invokeFuncSpec
    buildArgImplCtxSpec

reductionShowSpec :: Spec
reductionShowSpec = describe "Reduction (show instance)" $ do
    it "shows RedVal" $ do
        property $ \v -> v /= PzUnit ==> do
            show (RedVal PzUnit) `shouldBe` "RedVal PzUnit"
            show (RedVal v) `shouldBe` "RedVal (" ++ show v ++ ")"

    it "shows RedPush" $ do
        property $ \(ArbDict d) (Few vs) -> do
            show (RedPush (d, vs)) `shouldBe` "RedPush " ++ show (d, vs)

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

invokeFuncSpec :: Spec
invokeFuncSpec = describe "invokeFunc" $ do
    it "handles built-in func (quoted)" $ do
        property $ \(ArbDict d) impArgs args e es -> do
            let vs = unevalFuncCustom $ FuncCustom impArgs args e es
            invokeFunc d u (Func u u $ BodyBuiltIn $ symb "func") vs `shouldBe`
                Right (RedVal $ PzList [PzDict d, PzFunc d $ Func impArgs args $ BodyCustom e es])

    it "handles built-in func (evaled)" $ do
        property $ \v -> do
            invokeFunc u u (Func u u $ BodyBuiltIn $ symb "type_of") [v] `shouldBe` Right (RedVal $ _typeOf v)

    it "rejects custom func with invalid arity (quoted)" $ do
        property $ \(ArbDict d) impArgs (Few is) e es (Few vs) -> length is /= length vs ==> do
            let _ = vs :: [PzVal Quoted]
            invokeFunc d u (Func impArgs (ArgsArity is) $ BodyCustom e es) vs `shouldBe`
                Left ("Invalid number of arguments. Expected " ++ show (length is) ++ ", got: " ++ show (length vs))

    it "rejects custom func with invalid arity (evaled)" $ do
        property $ \(ArbDict d) impArgs (Few is) e es (Few vs) -> length is /= length vs ==> do
            let _ = vs :: [PzVal Evaled]
            invokeFunc d u (Func impArgs (ArgsArity is) $ BodyCustom e es) vs `shouldBe`
                Left ("Invalid number of arguments. Expected " ++ show (length is) ++ ", got: " ++ show (length vs))

    it "handles custom func (quoted)" $ do
        property $ \(ArbDict ctx) (ArbDict implCtx) impArgs args e es v (Few vs'') -> do
            let vs' = v:vs''
                vs_len = case args of ArgsArity is -> length is; _ -> length vs'
                vs = take vs_len $ concat $ repeat vs'
                argImplCtx = snd $ buildArgImplCtx ctx impArgs args $ map fromQuoted vs
            invokeFunc ctx implCtx (Func impArgs args $ BodyCustom e es) vs `shouldBe`
                Right (RedPush (M.union argImplCtx implCtx, e:es))

    it "handles custom func (evaled)" $ do
        property $ \(ArbDict ctx) (ArbDict implCtx) impArgs args e es v (Few vs'') -> do
            let vs' = v:vs''
                vs_len = case args of ArgsArity is -> length is; _ -> length vs'
                vs = take vs_len $ concat $ repeat vs'
                argImplCtx = snd $ buildArgImplCtx ctx impArgs args vs
            invokeFunc ctx implCtx (Func impArgs args $ BodyCustom e es) vs `shouldBe`
                Right (RedPush (M.union argImplCtx implCtx, e:es))


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