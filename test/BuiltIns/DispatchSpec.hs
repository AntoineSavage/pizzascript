module BuiltIns.DispatchSpec where

import Test.Hspec
import Test.QuickCheck

import BuiltIns.Dispatch
import BuiltIns.FuncImpls as Impls
import Control.Exception
import Eval
import Ops.PzValSpec
import Types.Numb
import Types.Func.FuncCustomSpec
import Types.PzVal
import Types.PzValSpec

spec :: Spec
spec = describe "dispatch" $ do
    it "dispatches to generic functions" $ do
        property $ \v1 v2 -> do
            dispatch undefined [v1] "type_of" `shouldBe` Right (Impls._typeOf v1)
            dispatch undefined [v1, v2] "eq" `shouldBe` Right (Impls._eq v1 v2)
            dispatch undefined [v1, v2] "lt" `shouldBe` Right (Impls._lt v1 v2)

    it "dispatches to semi-generic functions" $ do
        property $ \v -> do
            dispatch undefined [v] "is_empty" `shouldBe` Impls._isEmpty v
            dispatch undefined [v] "size" `shouldBe` Impls._size v

    it "dispatches to number functions" $ do
        property $ \(Positive n1) (Positive n2) -> do
            let f n = PzNum $ Numb $ fromIntegral $ (n :: Int) `mod` 10 + 1
                v1 = f n1
                v2 = f n2
            dispatch undefined [v1] "num" `shouldBe` Impls._num v1
            dispatch undefined [v1, v2] "add" `shouldBe` Impls._add v1 v2
            dispatch undefined [v1, v2] "sub" `shouldBe` Impls._sub v1 v2
            dispatch undefined [v1, v2] "mult" `shouldBe` Impls._mult v1 v2
            dispatch undefined [v1, v2] "div" `shouldBe` Impls._div v1 v2
            dispatch undefined [v1, v2] "rem" `shouldBe` Impls._rem v1 v2
            dispatch undefined [v1, v2] "exp" `shouldBe` Impls._exp v1 v2
            dispatch undefined [v1, v2] "log" `shouldBe` Impls._log v1 v2
            dispatch undefined [v1] "round" `shouldBe` Impls._round v1
            dispatch undefined [v1] "floor" `shouldBe` Impls._floor v1
            dispatch undefined [v1] "ceil" `shouldBe` Impls._ceil v1
            dispatch undefined [v1] "trunc" `shouldBe` Impls._trunc v1

    it "dispatches to boolean functions" $ do
        property $ \v1 v2 -> do
            dispatch undefined [v1] "not" `shouldBe` Right (Impls._not v1)
            dispatch undefined [v1, v2] "or" `shouldBe` Right (Impls._or v1 v2)
            dispatch undefined [v1, v2] "and" `shouldBe` Right (Impls._and v1 v2)

    it "dispatches to dictionary functions" $ do
        property $ \v1 -> do
            dispatch undefined [v1] "keys" `shouldBe` Impls._keys v1
            dispatch undefined [v1] "assocs" `shouldBe` Impls._assocs v1

    it "dispatches to func functions" $ do
        property $ \v1 v2 (ArbDict d) vs -> do
            dispatch d vs "func" `shouldBe` Impls._func d vs
            dispatch undefined [v1] "get_impl_ctx" `shouldBe` Impls._getImplCtx v1
            dispatch undefined [v1, v2] "set_impl_ctx" `shouldBe` Impls._setImplCtx v1 v2
            dispatch undefined [v1] "get_expl_ctx" `shouldBe` Impls._getExplCtx v1
            dispatch undefined [v1] "get_arg_pass" `shouldBe` Impls._getArgPass v1
            dispatch undefined [v1] "get_args" `shouldBe` Impls._getArgs v1
            dispatch undefined [v1] "get_body" `shouldBe` Impls._getBody v1

    it "rejects unsupported built-in function" $ do
        property $ \nameSuffix -> do
            let funcName = "$" ++ nameSuffix
            evaluate (dispatch undefined [] funcName) `shouldThrow`
                errorCall ("Built-in function '" ++ funcName ++ "' not supported")