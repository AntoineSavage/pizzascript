module ReduceSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as M

import BuiltIns.FuncImpls
import Control.Monad
import Data.Either
import Eval
import InvokeFunc
import Ops.PzVal
import Ops.StackFrame
import Ops.Symb
import Quote
import Reduce
import TestUtils
import Types.Func
import Types.Func.ArgPass
import Types.Func.FuncArgs
import Types.Func.FuncBody
import Types.Func.FuncImpureArgs
import Types.PzVal
import Types.PzValSpec
import Types.StackFrame
import Types.StackFrameSpec

spec :: Spec
spec = do
    accSpec
    reduceSpec
    reduceBlockSpec
    reduceFormQuotedSpec
    reduceFormEvaledSpec
    reduceInvocArgsSpec
    reduceInvocEvaledSpec
    reduceInvocSpec
    toAccSpec
    toPzValSpec

accSpec :: Spec
accSpec = describe "Acc" $ do
    it "implements Show" $ do
        property $ \v (Few fs) -> do
            show (Acc Nothing fs) `shouldBe` "Acc Nothing " ++ show fs
            show (Acc (Just v) fs) `shouldBe` "Acc (" ++ show (Just v) ++ ") " ++ show fs

    it "implements Eq" $ do
        property $ \mvx mvy (Few fsx) (Few fsy) -> do
            Acc mvx fsx == Acc mvx fsx `shouldBe` True
            Acc mvx fsx == Acc mvy fsx `shouldBe` mvx == mvy
            Acc mvx fsx == Acc mvx fsy `shouldBe` fsx == fsy

reduceSpec :: Spec
reduceSpec = describe "reduce" $ do
    it "handles empty stack frames" $ do
        property $ \mv -> do
            let acc = Acc mv []
            reduce acc `shouldBe` Right acc

    it "reduces block" $ do
        property $ \mv (ArbDict ctx) (Few vs) (Few fs) -> do
            reduce (Acc mv $ StackFrame ctx (Block vs) :fs) `shouldBe` reduceBlock ctx vs (Acc mv fs)

    it "reduces form (quoted)" $ do
        property $ \mv (ArbDict ctx) v (Few vs) (Few fs) -> do
            reduce (Acc mv $ StackFrame ctx (FormQuoted v vs) :fs) `shouldBe` reduceFormQuoted ctx v vs (Acc mv fs)

    it "reduces form (evaled)" $ do
        property $ \mv (ArbDict ctx) v (Few vs) (Few fs) -> do
            reduce (Acc mv $ StackFrame ctx (FormEvaled v vs) :fs) `shouldBe` reduceFormEvaled ctx v vs (Acc mv fs)

    it "reduces invoc (quoted)" $ do
        property $ \mv (ArbDict ctx) ic impArgs args e es (Few vs) (Few fs) -> do
            let f1 = Func impArgs args $ BodyBuiltIn $ symb "func"
                f2 = Func impArgs args $ BodyCustom e es
            reduce (Acc mv $ StackFrame ctx (InvocQuoted ic f1 vs) :fs) `shouldBe` reduceInvoc ctx ic f1 vs fs
            reduce (Acc mv $ StackFrame ctx (InvocQuoted ic f2 vs) :fs) `shouldBe` reduceInvoc ctx ic f2 vs fs

    it "reduces invoc (args)" $ do
        property $ \mv (ArbDict ctx) ic f (Few vs) (Few qvs) (Few fs) -> do
            reduce (Acc mv $ StackFrame ctx (InvocArgs ic f vs qvs) :fs) `shouldBe` reduceInvocArgs ctx ic f vs qvs (Acc mv fs)

    it "reduces invoc (evaled)" $ do
        property $ \mv (ArbDict ctx) ic impArgs args (Few vs) (Few fs) -> do
            let f = Func impArgs args $ BodyBuiltIn $ symb "type_of"
            reduce (Acc mv $ StackFrame ctx (InvocEvaled ic f vs) :fs) `shouldBe` reduceInvocEvaled ctx ic f vs (Acc mv fs)

reduceBlockSpec :: Spec
reduceBlockSpec = describe "reduceBlock" $ do
    it "handles empty block" $ do
        property $ \mv (Few fs) -> do
            let acc = Acc mv fs
            reduceBlock u [] acc `shouldBe` Right acc

    it "evals next block element" $ do
        property $ \(ArbDict ctx) n (Few vs') (Few fs) -> do
            let v = PzNum n
                vs = v:vs'
            reduceBlock ctx vs (Acc u fs) `shouldBe` Right (Acc (Just v) $ StackFrame ctx (Block vs') : fs)

reduceFormQuotedSpec :: Spec
reduceFormQuotedSpec = describe "reduceFormQuoted" $ do
    it "evals first form element" $ do
        property $ \(ArbDict ctx) n (Few vs) (Few fs) -> do
            let v = PzNum n
            reduceFormQuoted ctx v vs (Acc Nothing fs) `shouldBe`
                Right (Acc (Just v) $ StackFrame ctx (FormQuoted v vs) :fs)

    it "handles evaled first form element" $ do
        property $ \(ArbDict ctx) n (Few vs) (Few fs) -> do
            reduceFormQuoted ctx u vs (Acc (Just $ PzNum n) fs) `shouldBe`
                Right (Acc Nothing $ StackFrame ctx (FormEvaled (PzNum n) vs) :fs)

reduceFormEvaledSpec :: Spec
reduceFormEvaledSpec = describe "reduceFormEvaled" $ do
    it "rejects malformed function invocation" $ do
        property $ \n s sym (Few xs) (ArbDict ctx) (Few vs) (Few fs) -> do
            forM_   [ PzUnit, PzNum n, PzStr s, PzSymb sym
                    , PzList xs, PzDict ctx
                    ] $ \v -> do
                reduceFormEvaled ctx v vs (Acc u fs) `shouldBe`
                    (Left $
                        "Error: Malformed function invocation (first form element must be a function)"
                        ++ "\n was: " ++ show v
                    )

    it "handles eval function invocation" $ do
        property $ \(ArbDict ctx) (ArbDict ic) s args body (Few vs) (Few fs) -> do
            forM_   [ Func None args body
                    , Func (ArgPass Eval) args body
                    , Func (Both Eval s) args body
                    ] $ \f -> do
                reduceFormEvaled ctx (PzFunc ic f) vs (Acc u fs) `shouldBe`
                    Right (Acc Nothing $ (StackFrame ctx $ InvocArgs ic f [] vs) :fs)

    it "handles quote function invocation" $ do
        property $ \(ArbDict ctx) (ArbDict ic) s args body (Few vs) (Few fs) -> do
            forM_   [ Func (ArgPass Quote) args body
                    , Func (Both Quote s) args body
                    ] $ \f -> do
                reduceFormEvaled ctx (PzFunc ic f) vs (Acc u fs) `shouldBe`
                    Right (Acc Nothing $ (StackFrame ctx $ InvocQuoted ic f vs) :fs)

    it "rejects unquote function invocation" $ do
        property $ \(ArbDict ctx) (ArbDict ic) s args body (Few vs) (Few fs) -> do
            forM_   [ Func (ArgPass Unquote) args body
                    , Func (Both Unquote s) args body
                    ] $ \f -> do
                reduceFormEvaled ctx (PzFunc ic f) vs (Acc u fs) `shouldBe`
                    Right (Acc Nothing $ (StackFrame ctx $ InvocArgs ic f [] vs) :fs)

    it "rejects deep-quote function invocation" $ do
        property $ \(ArbDict ctx) (ArbDict ic) s args body (Few vs) (Few fs) -> do
            forM_   [ Func (ArgPass DeepQuote) args body
                    , Func (Both DeepQuote s) args body
                    ] $ \f -> do
                reduceFormEvaled ctx (PzFunc ic f) vs (Acc u fs) `shouldBe`
                    Right (Acc Nothing $ (StackFrame ctx $ InvocArgs ic f [] vs) :fs)

    it "rejects deep-unquote function invocation" $ do
        property $ \(ArbDict ctx) (ArbDict ic) s args body (Few vs) (Few fs) -> do
            forM_   [ Func (ArgPass DeepUnquote) args body
                    , Func (Both DeepUnquote s) args body
                    ] $ \f -> do
                reduceFormEvaled ctx (PzFunc ic f) vs (Acc u fs) `shouldBe`
                    Right (Acc Nothing $ (StackFrame ctx $ InvocArgs ic f [] vs) :fs)

reduceInvocArgsSpec :: Spec
reduceInvocArgsSpec = describe "reduceInvocArgs" $ do
    it "handles single evaluated arg" $ do
        property $ \(ArbDict ctx) (ArbDict ic) f v (Few vs) (Few qvs) (Few fs) -> do
            reduceInvocArgs ctx ic f vs qvs (Acc (Just v) fs) `shouldBe`
                Right (Acc Nothing $ StackFrame ctx (InvocArgs ic f (v:vs) qvs) : fs)

    it "handles all evaluated args" $ do
        property $ \(ArbDict ctx) (ArbDict ic) f (Few vs) (Few fs) -> do
            reduceInvocArgs ctx ic f vs [] (Acc Nothing fs) `shouldBe`
                Right (Acc Nothing $ StackFrame ctx (InvocEvaled ic f $ reverse vs) : fs)

    it "evaluates single arg (Eval)" $ do
        property $ \(ArbDict ctx) (ArbDict ic) args s (Few vs) qv (Few qvs') (Few fs) -> do
            let f = Func (ArgPass Eval) args $ BodyBuiltIn s
                qvs = qv:qvs'
            reduceInvocArgs ctx ic f vs qvs (Acc Nothing fs) `shouldBe`
                (eval ctx qv >>= toAcc ctx fs (InvocArgs ic f vs qvs'))

    it "evaluates single arg (Quote)" $ do
        property $ \(ArbDict ctx) (ArbDict ic) args s (Few vs) qv (Few qvs') (Few fs) -> do
            let f = Func (ArgPass Quote) args $ BodyBuiltIn s
                qvs = qv:qvs'
            reduceInvocArgs ctx ic f vs qvs (Acc Nothing fs) `shouldBe`
                (toAcc ctx fs (InvocArgs ic f vs qvs') $ Evaled $ fromQuoted qv)

    it "evaluates single arg (Unquote)" $ do
        property $ \(ArbDict ctx) (ArbDict ic) args s (Few vs) qv (Few qvs') (Few fs) -> do
            let f = Func (ArgPass Unquote) args $ BodyBuiltIn s
                qvs = qv:qvs'
            reduceInvocArgs ctx ic f vs qvs (Acc Nothing fs) `shouldBe`
                (unquote qv >>= eval ctx >>= toAcc ctx fs (InvocArgs ic f vs qvs'))

    it "evaluates single arg (DeepQuote)" $ do
        property $ \(ArbDict ctx) (ArbDict ic) args s (Few vs) qv (Few qvs') (Few fs) -> do
            let f = Func (ArgPass DeepQuote) args $ BodyBuiltIn s
                qvs = qv:qvs'
            reduceInvocArgs ctx ic f vs qvs (Acc Nothing fs) `shouldBe`
                (eval ctx qv >>= toAcc ctx fs (InvocArgs ic f vs qvs') . Evaled . fromQuoted . toPzVal)

    it "evaluates single arg (DeepUnquote)" $ do
        property $ \(ArbDict ctx) (ArbDict ic) args s (Few vs) qv (Few qvs') (Few fs) -> do
            let f = Func (ArgPass DeepUnquote) args $ BodyBuiltIn s
                qvs = qv:qvs'
            reduceInvocArgs ctx ic f vs qvs (Acc Nothing fs) `shouldBe`
                (eval ctx qv >>= unquote . toPzVal >>= eval ctx >>= toAcc ctx fs (InvocArgs ic f vs qvs'))

reduceInvocEvaledSpec :: Spec
reduceInvocEvaledSpec = describe "reduceInvocEvaled" $ do
    it "invokes function (built-in)" $ do
        property $ \(ArbDict ctx) (ArbDict ic) impArgs args v (Few fs) -> do
            let f = Func impArgs args $ BodyBuiltIn $ symb "type_of"
            reduceInvocEvaled ctx ic f [v] (Acc Nothing fs) `shouldBe` Right (Acc (Just $ _typeOf v) $ StackFrame ctx (InvocEvaled ic f [v]) : fs)

    it "invokes function (custom)" $ do
        property $ \(ArbDict ctx) (ArbDict ic) impArgs s e es (Few vs) (Few fs) -> do
            let f = Func impArgs (ArgsVaria s) $ BodyCustom e es
            reduceInvocEvaled ctx ic f vs (Acc Nothing fs) `shouldBe` reduceInvoc ctx ic f vs (StackFrame ctx (InvocEvaled ic f vs) : fs)

    it "handles pure function output" $ do
        property $ \(ArbDict ctx) (ArbDict ic) args body v (Few vs) (Few fs) -> do
            let acc = Acc (Just v) fs
            reduceInvocEvaled ctx ic (Func None args body) vs acc `shouldBe` Right acc
            reduceInvocEvaled ctx ic (Func (ArgPass u) args body) vs acc `shouldBe` Right acc

    it "handles impure valid function output" $ do
        property $ \(ArbDict ctx) (ArbDict ic) args body ctx' r (Few vs) (Few fs) -> do
            let acc = Acc (Just $ PzList [PzDict ctx', r]) fs
            reduceInvocEvaled ctx ic (Func (Both u u) args body) vs acc `shouldBe` Right (Acc (Just r) $ setCtx ctx' fs)

    it "rejects impure invalid function output" $ do
        property $ \n s sym (ArbDict d) f (ArbDict ctx) (ArbDict ic) args body (Few vs) (Few fs) -> do
            forM_   [ PzUnit, PzNum n, PzStr s, PzSymb sym
                    , PzList [], PzList [PzUnit], PzList [PzUnit, PzUnit], PzList [PzUnit, PzUnit, PzUnit]
                    , PzDict d, PzFunc d f
                    ] $ \r -> do
                reduceInvocEvaled ctx ic (Func (Both u u) args body) vs (Acc (Just r) fs) `shouldBe`
                    (Left $
                        "Error: Invalid impure function output. Must be a size-2 list containing (in order):"
                        ++ "\n 1) the output context (a dictionary)"
                        ++ "\n 2) the normal return value (any type)"
                        ++ "\n was: " ++ show r
                    )

reduceInvocSpec :: Spec
reduceInvocSpec = describe "reduceInvoc" $ do
    it "handles built-in function invocation" $ do
        property $ \(ArbDict ctx) (ArbDict ic) v (Few fs) -> do
            let f = Func u u $ BodyBuiltIn $ symb "type_of"
            reduceInvoc ctx ic f [v] fs `shouldBe` Right (Acc (Just $ _typeOf v) fs)

    it "handles custom function invocation" $ do
        property $ \(ArbDict ctx) (ArbDict ic) impArgs s e es (Few vs) (Few fs) -> do
            let _ = vs :: [PzVal Evaled]
                f = Func impArgs (ArgsVaria s) $ BodyCustom e es
                Right (ResultPushBlock (ctx', _)) = invokeFunc ctx ic f vs
            reduceInvoc ctx ic f vs fs `shouldBe` Right (Acc Nothing $ StackFrame ctx' (Block $ e:es) : fs)

toAccSpec :: Spec
toAccSpec = describe "toAcc" $ do
    it "handles Evaled" $ do
        property $ \(ArbDict d) (Few fs) spec v -> do
            toAcc d fs spec (Evaled v) `shouldBe`
                Right (Acc (Just v) $ StackFrame d spec : fs)
    
    it "handles PushForm" $ do
        property $ \(ArbDict d) (Few fs) spec v vs -> do
            toAcc d fs spec (PushForm v vs) `shouldBe`
                Right (Acc Nothing $ StackFrame d (FormQuoted v vs) : StackFrame d spec : fs)

toPzValSpec :: Spec
toPzValSpec = describe "toPzVal" $ do
    it "unevals Evaled" $ do
        property $ \v -> do
            toPzVal (Evaled v) `shouldBe` uneval v

    it "returns form as is" $ do
        property $ \v (Few vs) -> do
            toPzVal (PushForm v vs) `shouldBe` PzList (v:vs)