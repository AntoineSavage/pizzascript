module EvalSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as M

import Control.Exception
import Control.Monad
import Data.Either
import Data.List
import Eval
import Ops.Func.ArgPass
import Ops.Func.FuncCustomSpec
import Ops.PzVal
import Ops.PzValSpec
import Ops.Symb
import Ops.SymbSpec
import Symbs
import TestUtils
import Types.Func
import Types.Func.ArgPass
import Types.Func.FuncArgs
import Types.Func.FuncBody
import Types.Func.FuncCustom
import Types.Func.FuncImpureArgs
import Types.Nat
import Types.Numb
import Types.PzVal
import Types.Str
import Types.Symb
import Utils

spec :: Spec
spec = do
    evalResultShowSpec
    evalVsUnevalSpec
    evalSpec
    unevalSpec
    evalFuncCustomVsUnevalFuncCustomSpec
    evalFuncCustomSpec
    unevalFuncCustomSpec
    evalImpureArgsVsUnevalImpureArgsSpec
    evalImpureArgsSpec
    unevalImpureArgsSpec
    evalArgsVsUnevalArgsSpec
    evalArgsSpec
    unevalArgsSpec
    evalQuotedIdentSpec
    validateNoDuplicateQuotedIdentsSpec
    unconsFuncBodySpec
    getQuotedIdentSpec

evalResultShowSpec :: Spec
evalResultShowSpec = describe "EvalResult (Show instance" $ do
    it "shows Evaled" $ do
        show (Evaled PzUnit) `shouldBe` "Evaled PzUnit"

    it "shows Form" $ do
        show (Form PzUnit []) `shouldBe` "Form PzUnit []"

evalVsUnevalSpec :: Spec
evalVsUnevalSpec = describe "eval vs uneval" $ do
    it "composes eval and uneval into id (unit)" $ do
        let v = PzUnit
        eval undefined (uneval v) `shouldBe` Right (Evaled v)

    it "composes eval and uneval into id (num)" $ do
        property $ \n -> do
            let v = PzNum n
            eval undefined (uneval v) `shouldBe` Right (Evaled v)

    it "composes eval and uneval into id (str)" $ do
        property $ \s -> do
            let v = PzStr s
            eval undefined (uneval v) `shouldBe` Right (Evaled v)

    it "composes eval and uneval into id (symb)" $ do
        property $ \s -> do
            let v = PzSymb s
            eval undefined (uneval v) `shouldBe` Right (Evaled v)

    it "composes eval and uneval into id (built-in func)" $ do
        property $ \(ArbDict c) (ArbDict d) impArgs args (QuotedIdent s) -> do
            let v = PzFunc d $ Func impArgs args $ BodyBuiltIn s
                ctx = M.insert (PzSymb s) v c
            eval ctx (uneval v) `shouldBe` Right (Evaled v)

    it "converts lists, dictionaries and custom funcs to Forms" $ do
        property $ \(Few l) (ArbDict d) impArgs args x xs -> do
            let isForm (Right (Form _ _)) = True
                isForm _                = False
            isForm (eval undefined (uneval $ PzList l)) `shouldBe` True
            isForm (eval undefined (uneval $ PzDict d)) `shouldBe` True
            isForm (eval undefined (uneval $ PzFunc d $ Func impArgs args $ BodyCustom x xs)) `shouldBe` True

evalSpec :: Spec
evalSpec = describe "eval" $ do
    it "rejects the unit type" $ do
        let v = PzUnit
        evaluate (eval undefined v) `shouldThrow` errorCall ("Can only evaluate quoted values: " ++ show v)

    it "converts number to itself" $ do
        property $ \d -> do
            eval undefined (PzNum d) `shouldBe` Right (Evaled $ PzNum d)

    it "converts string to itself" $ do
        property $ \s -> do
            eval undefined (PzStr s) `shouldBe` Right (Evaled $ PzStr s)

    it "returns value associated to quoted identifier" $ do
        property $ \(ArbDict c) (Ident f ns) v -> do
            let k = PzSymb $ Symb Z f ns
            eval (M.insert k v c) k `shouldBe` Right (Evaled v)

    it "undefined quoted identifier" $ do
        property $ \(ArbDict c) (Ident f ns) -> do
            let k = PzSymb $ Symb Z f ns
                ctx = M.delete k c
            leftAsStr (eval ctx k) `shouldContain`
                (show $ "Error: Undefined identifier: " ++ unparseSymb (Symb Z f ns)
                    ++ "\n context keys: " ++ show (M.keys ctx)
                )

    it "converts symbol to itself, unquoted" $ do
        property $ \n (Ident f ns) -> do
            let v = PzSymb $ Symb (S n) f ns
            eval undefined v `shouldBe` Right (Evaled $ PzSymb $ Symb n f ns)

    it "converts empty list to the unit type" $ do
        let v = PzList []
        eval undefined v `shouldBe` Right (Evaled PzUnit)

    it "converts non-empty list to a form" $ do
        property $ \x xs -> do
            eval undefined (PzList $ x:xs) `shouldBe` Right (Form x xs)

    it "rejects dictionaries" $ do
        property $ \(ArbDict d) -> do
            let v = PzDict d
            evaluate (eval undefined v) `shouldThrow` errorCall ("Can only evaluate quoted values: " ++ show v)

    it "rejects functions" $ do
        property $ \(ArbDict d) f -> do
            let v = PzFunc d f
            evaluate (eval undefined v) `shouldThrow` errorCall ("Can only evaluate quoted values: " ++ show v)

unevalSpec :: Spec
unevalSpec = describe "uneval" $ do
    it "converts unit to empty form" $ do
        uneval PzUnit `shouldBe` (PzList [])

    it "converts number to itself" $ do
        property $ \d -> do
            uneval (PzNum d) `shouldBe` (PzNum d)

    it "converts string to itself" $ do
        property $ \s -> do
            uneval (PzStr s) `shouldBe` (PzStr s)

    it "converts symbol to itself, quoted" $ do
        property $ \s -> do
            uneval (PzSymb s) `shouldBe` (PzSymb $ quoteSymb s)

    it "converts list to itself, unevaled recursively, prepended with list symbol" $ do
        property $ \(Few l) -> do
            uneval (PzList l) `shouldBe` (PzList $ (pzSymbList:) $ map uneval l)

    it "converts dict to list, unevaled recursively, prepended with dict symbol" $ do
        property $ \(ArbDict d) -> do
            uneval (PzDict d) `shouldBe` (PzList $ (pzSymbDict:) $ flip map (M.assocs d) $
                \(k, v) -> PzList $ [uneval k, uneval v])

    it "converts built-in function to quoted identifier" $ do
        property $ \(ArbDict implCtx) impArgs args s -> do
            uneval (PzFunc implCtx $ Func impArgs args $ BodyBuiltIn s) `shouldBe` (PzSymb s)

    it "converts custom function to list" $ do
        property $ \(ArbDict implCtx) f@(FuncCustom impArgs args x xs) -> do
            uneval (PzFunc implCtx $ Func impArgs args $ BodyCustom x xs) `shouldBe` (PzList $ unevalFuncCustom f)

evalFuncCustomVsUnevalFuncCustomSpec :: Spec
evalFuncCustomVsUnevalFuncCustomSpec = describe "evalFuncCustom vs unevalFuncCustom" $ do
    it "composes evalFuncCustom and unevalFuncCustom into id" $ do
        property $ \func -> do
            let elems = unevalFuncCustom func
            evalFuncCustom elems `shouldBe` Right func
            unevalFuncCustom <$> evalFuncCustom elems `shouldBe` Right elems

evalFuncCustomSpec :: Spec
evalFuncCustomSpec = describe "evalFuncCustom" $ do
    it "converts custom function" $ do
        property $ \func@(FuncCustom impArgs args x xs) -> do
            let elems = unevalImpureArgs impArgs ++ unevalArgs args ++ [x] ++ xs
            evalFuncCustom elems `shouldBe` Right func

    it "rejects empty elems" $ do
        leftAsStr (evalFuncCustom []) `shouldContain`
            (show $ "Error: Function argument definition must be either:\n"
            ++ " - a single varargs identifier\n"
            ++ " - an form of arity identifiers\n"
            ++ " was: []")

unevalFuncCustomSpec :: Spec
unevalFuncCustomSpec = describe "unevalFuncCustom" $ do
    it "converts custom function" $ do
        property $ \func@(FuncCustom impArgs args x xs) -> do
            unevalFuncCustom func `shouldBe` unevalImpureArgs impArgs ++ unevalArgs args ++ [x] ++ xs

evalImpureArgsVsUnevalImpureArgsSpec :: Spec
evalImpureArgsVsUnevalImpureArgsSpec = describe "evalImpureArgs vs unevalImpureArgs" $ do
    it "composes evalImpureArgs and unevalImpureArgs into id" $ do
        property $ \impArgs n (Few es') -> do
            let es = PzNum n : es'
                elems = unevalImpureArgs impArgs
            evalImpureArgs (elems ++ es) `shouldBe` Right (impArgs, es)
            unevalImpureArgs <$> fst <$> evalImpureArgs (elems ++ es) `shouldBe` Right elems

evalImpureArgsSpec :: Spec
evalImpureArgsSpec = describe "evalImpureArgs" $ do
    it "converts mismatch to None" $ do
        forM_ [ []
                , [PzNum $ Numb 0]
                , [PzStr $ Str ""]
                , [PzSymb $ symb "_"]
                , [PzList $ []]
                , [PzList $ [PzNum $ Numb 0]]
                , [PzList $ [PzStr $ Str ""]]
                , [PzList $ [PzSymb $ symb "_"]]
                , [PzList $ [PzList []]]
            ] $ \es -> do
            evalImpureArgs es `shouldBe` Right (None, es)

    it "converts singleton form to ArgPass" $ do
        property $ \ap (Few es) -> do
            let elems = PzList [PzSymb $ argPassToSymb ap] : es
            evalImpureArgs elems `shouldBe` Right (ArgPass ap, es)

    it "converts size-2 form to Both" $ do
        property $ \ap (QuotedIdent ec) (Few es) -> do
            let elems = PzList [PzSymb $ argPassToSymb ap, PzSymb ec] : es
            evalImpureArgs elems `shouldBe` Right (Both ap ec, es)

    it "rejects invalid arg-pass symbol" $ do
        property $ \s (Few es) -> do
            let sym = Symb (S Z) '_' s
                elems = PzList [PzSymb $ sym] : es
            leftAsStr (evalImpureArgs elems) `shouldContain`
                (show $ "Error: Invalid argument-passing behaviour symbol: " ++ show sym)

    it "rejects size-3 (or more) form" $ do
        property $ \ap (QuotedIdent ec) a (Few as) (Few es) -> do
            let elems = (PzList $ [ PzSymb $ argPassToSymb ap, PzSymb ec] ++ [a] ++ as) : es
            leftAsStr (evalImpureArgs elems) `shouldContain`
                (show $ "Error: Impure function argument definition must be either:\n"
                    ++ " - a valid argument-passsing behaviour symbol only\n"
                    ++ " - a valid argument-passsing behaviour symbol, followed by an identifier\n"
                    ++ " was: " ++ show elems
                )

unevalImpureArgsSpec :: Spec
unevalImpureArgsSpec = describe "unevalImpureArgs" $ do
    it "converts None to empty list" $ do
        unevalImpureArgs None `shouldBe` []

    it "converts ArgPass to singleton list" $ do
        property $ \ap -> do
            unevalImpureArgs (ArgPass ap) `shouldBe` [PzList [PzSymb $ argPassToSymb ap]]

    it "converts Both to size-2 list" $ do
        property $ \ap (QuotedIdent ec) -> do
            unevalImpureArgs (Both ap ec) `shouldBe` [PzList [PzSymb $ argPassToSymb ap, PzSymb ec]]

evalArgsVsUnevalArgsSpec :: Spec
evalArgsVsUnevalArgsSpec = describe "evalArgs vs unevalArgs" $ do
    it "composes evalArgs and unevalArgs into id" $ do
        property $ \args (Few es) -> do
            let elems = unevalArgs args
            evalArgs (elems ++ es) `shouldBe` Right (args, es)
            unevalArgs <$> fst <$> evalArgs (elems ++ es) `shouldBe` Right elems

evalArgsSpec :: Spec
evalArgsSpec = describe "evalArgs" $ do
    it "converts variadic args ident" $ do
        property $ \(QuotedIdent s) (Few es) -> do
            let elems = PzSymb s : es
            evalArgs elems `shouldBe` Right (ArgsVaria s, es)

    it "converts arity args idents" $ do
        property $ \(QuotedIdents ss) (Few es) -> do
            let elems = (PzList $ map PzSymb ss) : es
            evalArgs elems `shouldBe` Right (ArgsArity ss, es)

    it "rejects empty list" $ do
        leftAsStr (evalArgs []) `shouldContain`
            (show $ "Error: Function argument definition must be either:\n"
                ++ " - a single varargs identifier\n"
                ++ " - an form of arity identifiers\n"
                ++ " was: []"
            )

    it "rejects non-ident and non-form list" $ do
        property $ \(Few es) -> do
            forM_ [ PzNum $ Numb 0, PzStr $ Str "" ] $ \e ->
                leftAsStr (evalArgs $ e:es) `shouldContain`
                    (show $ "Error: Function argument definition must be either:\n"
                        ++ " - a single varargs identifier\n"
                        ++ " - an form of arity identifiers\n"
                        ++ " was: " ++ show (e:es)
                    )

unevalArgsSpec :: Spec
unevalArgsSpec = describe "unevalArgs" $ do
    it "converts variadic args ident" $ do
        property $ \s -> do
            unevalArgs (ArgsVaria s) `shouldBe` [PzSymb s]

    it "converts arity args idents" $ do
        property $ \(Few ss) -> do
            unevalArgs (ArgsArity ss) `shouldBe` [PzList $ map PzSymb ss]

evalQuotedIdentSpec :: Spec
evalQuotedIdentSpec = describe "evalQuotedIdent" $ do
    it "returns defined identifier" $ do
        property $ \(ArbDict c) s v -> do
            let k = PzSymb s
            evalQuotedIdent (M.insert k v c) k `shouldBe` Right v

    it "rejects undefined identifier" $ do
        property $ \(ArbDict c) s -> do
            let k = PzSymb s
                ctx = M.delete k c
            leftAsStr (evalQuotedIdent ctx k) `shouldContain`
                    (show $ "Error: Undefined identifier: " ++ unparseSymb s ++ "\n context keys: " ++ show (M.keys ctx))

validateNoDuplicateQuotedIdentsSpec :: Spec
validateNoDuplicateQuotedIdentsSpec = describe "validateNoDuplicateQuotedIdents" $ do
    it "accepts zero args" $ do
        forM_ [none, form] $ \impArgs -> do
            validateNoDuplicateQuotedIdents impArgs (ArgsArity []) `shouldBe` Right ()
            validateNoDuplicateQuotedIdents impArgs (ArgsArity []) `shouldBe` Right ()

    it "accepts one arg (impure args)" $ do
        property $ \(QuotedIdent ctx) -> do
            validateNoDuplicateQuotedIdents (both ctx) (ArgsArity []) `shouldBe` Right ()

    it "accepts one arg (varia)" $ do
        property $ \(QuotedIdent varargs) -> do
            forM_ [none, form] $ \impArgs -> do
                validateNoDuplicateQuotedIdents impArgs (ArgsVaria varargs) `shouldBe` Right ()

    it "accepts one arg (arity)" $ do
        property $ \(QuotedIdent arg) -> do
            forM_ [none, form] $ \impArgs -> do
                validateNoDuplicateQuotedIdents impArgs (ArgsArity [arg]) `shouldBe` Right ()

    it "accepts two args (impure+varia)" $ do
        property $ \(UniqueQuotedIdents2 ctx varargs) -> do
            validateNoDuplicateQuotedIdents (both ctx) (ArgsVaria varargs) `shouldBe` Right ()

    it "accepts two args (impure+arity)" $ do
        property $ \(UniqueQuotedIdents2 ctx arg) -> do
            validateNoDuplicateQuotedIdents (both ctx) (ArgsArity [arg]) `shouldBe` Right ()

    it "accepts two args (arity)" $ do
        property $ \(UniqueQuotedIdents2 arg1 arg2) -> do
            forM_ [none, form] $ \impArgs -> do
                validateNoDuplicateQuotedIdents impArgs (ArgsArity [arg1, arg2]) `shouldBe` Right ()

    it "accepts N+1 args (impure+arity)" $ do
        property $ \(UniqueQuotedIdentsNP1 ctx args) -> do
            validateNoDuplicateQuotedIdents (both ctx) (ArgsArity args) `shouldBe` Right ()

    it "rejects two args (impure+varia)" $ do
        property $ \(QuotedIdent x) -> do
            let (ctx, varargs) = (x, x)
            leftAsStr (validateNoDuplicateQuotedIdents (both ctx) (ArgsVaria varargs)) `shouldContain`
                    (show $ "Error: Duplicate identifiers in function definition: " ++ concatMap unparseSymb [x])

    it "rejects two args (impure+arity)" $ do
        property $ \(QuotedIdent x) -> do
            let (ctx, arg) = (x, x)
            leftAsStr (validateNoDuplicateQuotedIdents (both ctx) (ArgsArity [arg])) `shouldContain`
                    (show $ "Error: Duplicate identifiers in function definition: " ++ concatMap unparseSymb [x])

    it "rejects two args (arity)" $ do
        property $ \(QuotedIdent x) -> do
            let (arg1, arg2) = (x, x)
            forM_ [none, form] $ \impArgs -> do
                leftAsStr (validateNoDuplicateQuotedIdents none (ArgsArity [arg1, arg2])) `shouldContain`
                    (show $ "Error: Duplicate identifiers in function definition: " ++ concatMap unparseSymb [x])

    it "rejects N+1 args (impure+arity)" $ do
        property $ \(UniqueQuotedIdentsNP1 ctx args) -> do
            leftAsStr (validateNoDuplicateQuotedIdents (both ctx) (ArgsArity $ ctx:args)) `shouldContain`
                (show $ "Error: Duplicate identifiers in function definition: " ++ concatMap unparseSymb [ctx])

unconsFuncBodySpec :: Spec
unconsFuncBodySpec = describe "unconsFuncBody" $ do
    it "rejects empty body" $ do
        leftAsStr (unconsFuncBody []) `shouldContain`
            (show $ "Error: Function body must not be empty")
  
    it "uncons one element" $ do
        property $ \v -> do
            unconsFuncBody [v] `shouldBe` Right (v, [])
  
    it "uncons two elements" $ do
        property $ \v1 v2 -> do
            unconsFuncBody [v1, v2] `shouldBe` Right (v1, [v2])
  
    it "uncons three elements" $ do
        property $ \v1 v2 v3 -> do
            unconsFuncBody [v1, v2, v3] `shouldBe` Right (v1, [v2, v3])
  
    it "uncons N+1 elements" $ do
        property $ \v vs -> do
            unconsFuncBody (v:vs) `shouldBe` Right (v, vs)

getQuotedIdentSpec :: Spec
getQuotedIdentSpec = describe "getQuotedIdent" $ do
    it "converts quoted ident" $ do
        property $ \(QuotedIdent s) -> do
            getQuotedIdent (PzSymb s) `shouldBe` Right s

    it "rejects number" $ do
        property $ \n ->
            leftAsStr (getQuotedIdent (PzNum n)) `shouldContain`
                (show $ "Expected identifier\n was: " ++ show (PzNum n))

    it "rejects string" $ do
        property $ \s ->
            leftAsStr (getQuotedIdent (PzStr s)) `shouldContain`
                (show $ "Expected identifier\n was: " ++ show (PzStr s))

    it "rejects symbol" $ do
        property $ \(QuotedSymb s) ->
            leftAsStr (getQuotedIdent (PzSymb s)) `shouldContain`
                (show $ "Expected identifier\n was: " ++ show (PzSymb s))

    it "rejects list" $ do
        property $ \(Few l) ->
            leftAsStr (getQuotedIdent (PzList l)) `shouldContain`
                (show $ "Expected identifier\n was: " ++ show (PzList l))

-- Utils
none = None
form = ArgPass $ Eval
both = Both (Eval)

data UniqueQuotedIdents2 = UniqueQuotedIdents2 Symb Symb deriving (Show, Eq)
instance Arbitrary UniqueQuotedIdents2 where
    arbitrary = do
        QuotedIdent x <- arbitrary
        QuotedIdent y <- arbitrary
        if [x, y] /= nub [x, y] then arbitrary else
            return $ UniqueQuotedIdents2 x y

data UniqueQuotedIdentsNP1 = UniqueQuotedIdentsNP1 Symb [Symb] deriving (Show, Eq)
instance Arbitrary UniqueQuotedIdentsNP1 where
    arbitrary = do
        QuotedIdent x <- arbitrary
        xs <- arbFew $ do QuotedIdent x' <- arbitrary; return x'
        if x:xs /= nub (x:xs) then arbitrary else
            return $ UniqueQuotedIdentsNP1 x xs
