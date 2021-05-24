module EvalSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as M

import Control.Monad
import Data.AstExpr
import Data.Either
import Data.Func
import Data.Func.ArgPass
import Data.Func.ArgPassSpec
import Data.Func.FuncArgs
import Data.Func.FuncBody
import Data.Func.FuncCustom
import Data.Func.FuncCustomSpec
import Data.Func.FuncImpureArgs
import Data.Ident
import Data.Lst
import Data.Nat
import Data.Numb
import Data.NumbSpec
import Data.PzVal
import Data.PzValSpec
import Data.Str
import Data.StrSpec
import Data.Symb
import Data.SymbSpec
import Eval
import Quote
import QuoteSpec
import Text.Parsec.Pos 
import TestUtils
import Utils

spec :: Spec
spec = do
    evalExprVsUnevalExprSpec
    evalExprSpec
    unevalExprSpec
    evalFuncCustomVsUnevalFuncCustomSpec
    evalFuncCustomSpec
    unevalFuncCustomSpec
    evalImpureArgsVsUnevalImpureArgsSpec
    evalImpureArgsSpec
    unevalImpureArgsSpec
    evalArgsVsUnevalArgsSpec
    evalArgsSpec
    unevalArgsSpec
    evalIdentSpec
    validateNoDuplicateIdentsSpec

evalExprVsUnevalExprSpec :: Spec
evalExprVsUnevalExprSpec = describe "evalExpr vs unevalExpr (partial)" $ do
    it "composes evalExpr and unevalExpr into id (numbers)" $ do
        property $ \(ArbDict ctx) d -> do
            forM_ argPasses $ \eval -> do
                let v = PzNum d
                evalExpr ctx (unevalExpr v) eval `shouldBe` Right (Evaled v)

    it "composes evalExpr and unevalExpr into id (strings)" $ do
        property $ \(ArbDict ctx) s -> do
            forM_ argPasses $ \eval -> do
                let v = PzStr s
                evalExpr ctx (unevalExpr v) eval `shouldBe` Right (Evaled v)

    it "composes evalExpr and unevalExpr into id (symbols, Eval)" $ do
        property $ \(ArbDict ctx) s -> do
            let v = PzSymb s
            evalExpr ctx (unevalExpr v) Eval `shouldBe` Right (Evaled v)

    -- TODO: Unit
    -- TODO identifiers
    -- TODO symbols (except Eval)
    -- TODO lists

evalExprSpec :: Spec
evalExprSpec = describe "evalExpr" $ do
    it "evals numbers to themselves" $ do
        property $ \(ArbDict ctx) d -> do
            forM_ argPasses $ \eval -> do
                evalExpr ctx (AstNum d) eval `shouldBe`
                    Right (Evaled $ PzNum d)

    it "evals strings to themselves" $ do
        property $ \(ArbDict ctx) s -> do
            forM_ argPasses $ \eval -> do
                evalExpr ctx (AstStr s) eval `shouldBe`
                    Right (Evaled $ PzStr s)

    it "evals symbols (Eval) to themselves" $ do
        property $ \(ArbDict ctx) s -> do
            evalExpr ctx (AstSymb s) Eval `shouldBe`
                Right (Evaled $ PzSymb s)

    it "evals symbols (Quote or DeepQuote) as themselves with one more quote" $ do
        property $ \(ArbDict ctx) n i -> do
            forM_ [Quote, DeepQuote] $ \eval -> do
                evalExpr ctx (AstSymb $ Symb n i) eval `shouldBe`
                    Right (Evaled $ PzSymb $ Symb (S n) i)

    it "evals symbols (Unquote or DeepUnquote) as themselves with one less quote" $ do
        property $ \(ArbDict ctx) n i -> do
            forM_ [Unquote, DeepUnquote] $ \eval -> do
                evalExpr ctx (AstSymb $ Symb (S n) i) eval `shouldBe`
                    Right (Evaled $ PzSymb $ Symb n i)

    it "evals single-quoted symbols (Unquote or DeepUnquote) as the matching identifier, evaluated" $ do
        property $ \(ArbDict c) i v -> do
            let sym = symb i
                k = PzSymb sym
                ctx = M.insert k v c

            forM_ [Unquote, DeepUnquote] $ \eval -> do
                evalExpr ctx (AstSymb sym) eval `shouldBe`
                    Right (Evaled v)

    it "evals identifiers (Eval) as the associated value" $ do
        property $ \(ArbDict c) i v -> do
            let k = PzSymb $ symb i
                ctx = M.insert k v c

            evalExpr ctx (AstIdent i) Eval `shouldBe`
                Right (Evaled v)

    it "evals identifiers (Quote) as the corresponding symbol" $ do
        property $ \(ArbDict ctx) i -> do
            let sym = symb i
            evalExpr ctx (AstIdent i) Quote `shouldBe`
                Right (Evaled $ PzSymb sym)

    it "rejects identifiers (Unquote)" $ do
        property $ \(ArbDict ctx) i -> do
            isLeft (evalExpr ctx (AstIdent i) Unquote) `shouldBe` True

    it "evals identifiers (DeepQuote) as the associated value, quoted" $ do
        property $ \(ArbDict c) d s1 s2 n i' -> do
            let pairs =
                    [ (PzNum d, PzNum d)
                    , (PzStr s1, PzStr s1)
                    , (PzSymb $ Symb n i', PzSymb $ Symb (S n) i')
                    ]
            forM_ pairs $ \(v, v') -> do
                let i = Ident s2
                    k = PzSymb $ symb i
                    ctx = M.insert k v c

                evalExpr ctx (AstIdent i) DeepQuote `shouldBe`
                    Right (Evaled v')

    it "evals identifiers (DeepUnquote) as the associated value, unquoted" $ do
        property $ \(ArbDict c) d s1 s2 n i' -> do
            let pairs =
                    [ (PzNum d, PzNum d)
                    , (PzStr s1, PzStr s1)
                    , (PzSymb $ Symb (S n) i', PzSymb $ Symb n i')
                    ]
            forM_ pairs $ \(v', v) -> do
                let i = Ident s2
                    k = PzSymb $ symb i
                    ctx = M.insert k v' c

                evalExpr ctx (AstIdent i) DeepUnquote `shouldBe`
                    Right (Evaled v)

    it "evals lists (Eval) as the associated form" $ do
        property $ \(ArbDict ctx) k (Few es) -> do
            evalExpr ctx (AstList $ Lst k es) Eval `shouldBe`
                Right (ExprForm $ toForm k es)

    it "evals lists (Quote and DeepQuote) as themselves, quoted" $ do
        property $ \(ArbDict ctx) k (Few es) -> do
            forM_ [Quote, DeepQuote] $ \eval -> do
                evalExpr ctx (AstList $ Lst k es) eval `shouldBe`
                    evalExpr ctx (quote $ AstList $ Lst k es) Eval

    it "evals lists (Unquote and DeepUnquote) as themselves, unquoted" $ do
        property $ \(ArbDict ctx) k (UnquoteValids es) -> do
            forM_ [Unquote, DeepUnquote] $ \eval -> do
                evalExpr ctx (AstList $ Lst k es) eval `shouldBe`
                    (unquote (AstList $ Lst k es) >>= \e' -> evalExpr ctx e' Eval)

unevalExprSpec :: Spec
unevalExprSpec = describe "unevalExpr" $ do
    it "unevals unit to empty form" $ do
        unevalExpr PzUnit `shouldBe` (AstList $ Lst KindForm [])

    it "unevals number to itself" $ do
        property $ \d -> do
            unevalExpr (PzNum d) `shouldBe` (AstNum d)

    it "unevals string to itself" $ do
        property $ \s -> do
            unevalExpr (PzStr s) `shouldBe` (AstStr s)

    it "unevals symbol to itself" $ do
        property $ \s -> do
            unevalExpr (PzSymb s) `shouldBe` (AstSymb s)

    it "unevals list to itself" $ do
        property $ \(Few l) -> do
            unevalExpr (PzList l) `shouldBe` (AstList $ Lst KindList $ map unevalExpr l)

    it "unevals dict to itself" $ do
        property $ \(ArbDict d) -> do
            unevalExpr (PzDict d) `shouldBe`
                (AstList $ Lst KindDict $ flip map (M.assocs d) $ \(k, v) -> AstList $ Lst KindForm [unevalExpr k, unevalExpr v])

    it "unevals built-in function to identifier" $ do
        property $ \(ArbDict implCtx) impArgs args ident -> do
            unevalExpr (PzFunc implCtx $ Func impArgs args $ BodyBuiltIn ident) `shouldBe` (AstIdent ident)

    it "unevals custom function to list" $ do
        property $ \(ArbDict implCtx) f@(FuncCustom impArgs args body) -> do
            unevalExpr (PzFunc implCtx $ Func impArgs args $ BodyCustom body) `shouldBe` (AstList $ Lst KindForm $ unevalFuncCustom f)

evalFuncCustomVsUnevalFuncCustomSpec :: Spec
evalFuncCustomVsUnevalFuncCustomSpec = describe "evalFuncCustom vs unevalFuncCustom" $ do
    it "composes evalFuncCustom and unevalFuncCustom into id" $ do
        property $ \func -> do
            let elems = unevalFuncCustom func
            evalFuncCustom elems `shouldBe` Right func
            unevalFuncCustom <$> evalFuncCustom elems `shouldBe` Right elems

evalFuncCustomSpec :: Spec
evalFuncCustomSpec = describe "evalFuncCustom" $ do
    it "evals custom function" $ do
        property $ \func@(FuncCustom impArgs args body) -> do
            let elems = unevalImpureArgs impArgs ++ unevalArgs args ++ body
            evalFuncCustom elems `shouldBe` Right func

    it "rejects empty elems" $ do
        isLeft (evalFuncCustom []) `shouldBe` True

unevalFuncCustomSpec :: Spec
unevalFuncCustomSpec = describe "unevalFuncCustom" $ do
    it "unevals custom function" $ do
        property $ \func@(FuncCustom impArgs args body) -> do
            unevalFuncCustom func `shouldBe` unevalImpureArgs impArgs ++ unevalArgs args ++ body

evalImpureArgsVsUnevalImpureArgsSpec :: Spec
evalImpureArgsVsUnevalImpureArgsSpec = describe "evalImpureArgs vs unevalImpureArgs" $ do
    it "composes evalImpureArgs and unevalImpureArgs into id" $ do
        property $ \impArgs (Few es) -> do
            let elems = unevalImpureArgs impArgs
            evalImpureArgs (elems ++ es) `shouldBe` Right (impArgs, es)
            unevalImpureArgs <$> fst <$> evalImpureArgs (elems ++ es) `shouldBe` Right elems

evalImpureArgsSpec :: Spec
evalImpureArgsSpec = describe "evalImpureArgs" $ do
    it "evals mismatch to None" $ do
        forM_ [ []
                , [AstNum $ Numb 0]
                , [AstStr $ Str ""]
                , [AstIdent $ Ident ""]
                , [AstSymb $ symb $ Ident ""]
                , [AstList $ Lst KindList [AstSymb $ symb $ Ident ""]]
                , [AstList $ Lst KindDict [AstSymb $ symb $ Ident ""]]
                , [AstList $ Lst KindForm []]
            ] $ \es -> do
            evalImpureArgs es `shouldBe` Right (None, es)

    it "evals singleton form to ArgPass" $ do
        property $ \ap (Few es) -> do
            let elems = (AstList $ Lst KindForm [
                        AstSymb $ argPassToSymb ap
                    ]) : es
            evalImpureArgs elems `shouldBe` Right (ArgPass ap, es)

    it "evals size-2 form to Both" $ do
        property $ \ap s (Few es) -> do
            let ec = Ident [s]
                elems = (AstList $ Lst KindForm [
                        AstSymb $ argPassToSymb ap,
                        AstIdent ec
                    ]) : es
            evalImpureArgs elems `shouldBe` Right (Both ap ec, es)

    it "rejects invalid arg-pass symbol" $ do
        property $ \s (Few es) -> do
            let elems = (AstList $ Lst KindForm [
                        AstSymb $ symb $ Ident $ '_' : s
                    ]) : es
            isLeft (evalImpureArgs elems) `shouldBe` True

    it "rejects size-3 (or more) form" $ do
        property $ \ap ec a (Few as) (Few es) -> do
            let elems = (AstList $ Lst KindForm $ [
                        AstSymb $ argPassToSymb ap,
                        AstIdent ec
                    ] ++ [a] ++ as) : es
            isLeft (evalImpureArgs elems) `shouldBe` True

unevalImpureArgsSpec :: Spec
unevalImpureArgsSpec = describe "unevalImpureArgs" $ do
    it "unevals None to empty list" $ do
        unevalImpureArgs None `shouldBe` []

    it "unevals ArgPass to singleton list" $ do
        property $ \ap -> do
            unevalImpureArgs (ArgPass ap) `shouldBe`
                [AstList $ Lst KindForm [
                    AstSymb $ argPassToSymb ap
                ]]

    it "unevals Both to size-2 list" $ do
        property $ \ap ec -> do
            unevalImpureArgs (Both ap ec) `shouldBe`
                [AstList $ Lst KindForm [
                    AstSymb $ argPassToSymb ap,
                    AstIdent ec
                ]]

evalArgsVsUnevalArgsSpec :: Spec
evalArgsVsUnevalArgsSpec = describe "evalArgs vs unevalArgs" $ do
    it "composes evalArgs and unevalArgs into id" $ do
        property $ \args (Few es) -> do
            let elems = unevalArgs args
            evalArgs (elems ++ es) `shouldBe` Right (args, es)
            unevalArgs <$> fst <$> evalArgs (elems ++ es) `shouldBe` Right elems

evalArgsSpec :: Spec
evalArgsSpec = describe "evalArgs" $ do
    let toAstIdent = AstIdent . Ident
    it "evals variadic args ident" $ do
        property $ \s (Few es) -> do
            let elems = toAstIdent s : es
            evalArgs elems `shouldBe` Right (ArgsVaria $ Ident s, es)

    it "evals arity args idents" $ do
        property $ \ss (Few es) -> do
            let elems = (AstList $ Lst KindForm $ map toAstIdent ss) : es
            evalArgs elems `shouldBe` Right (ArgsArity $ map Ident ss, es)

    it "rejects empty list" $ do
        isLeft (evalArgs []) `shouldBe` True

    it "rejects non-ident and non-form list" $ do
        property $ \(Few es) -> do
            forM_   [ AstNum $ Numb 0, AstStr $ Str ""
                    , AstSymb $ symb $ Ident ""
                    , AstList $ Lst KindList []
                    , AstList $ Lst KindDict []
                    ] $ \e ->
                isLeft (evalArgs $ e:es) `shouldBe` True

unevalArgsSpec :: Spec
unevalArgsSpec = describe "unevalArgs" $ do
    it "unevals variadic args ident" $ do
        property $ \i -> do
            unevalArgs (ArgsVaria i) `shouldBe` [AstIdent i]

    it "unevals arity args idents" $ do
        property $ \is -> do
            unevalArgs (ArgsArity is) `shouldBe` [(AstList $ Lst KindForm $ map AstIdent is)]

evalIdentSpec :: Spec
evalIdentSpec = describe "evalIdent" $ do
    it "evaluates one ident part" $ do
        property $ \(ArbDict c) ident v -> do
            let k = PzSymb $ symb ident
                ctx = flip (M.insert k) c v
            evalIdent ctx ident `shouldBe` Right v

    it "evaluates undefined ident" $ do
        property $ \(ArbDict c) ident -> do
            let k = PzSymb $ symb ident
            isLeft (evalIdent (M.delete k c) ident) `shouldBe` True

validateNoDuplicateIdentsSpec :: Spec
validateNoDuplicateIdentsSpec = describe "validateNoDuplicateIdents" $ do
    it "accepts zero args" $ do
        forM_ [none, form] $ \impArgs -> do
            validateNoDuplicateIdents impArgs (ArgsArity []) `shouldBe` Right ()
            validateNoDuplicateIdents impArgs (ArgsArity []) `shouldBe` Right ()

    it "accepts one arg (impure args)" $ do
        property $ \c -> do
            let ctx = Ident c
            validateNoDuplicateIdents (both ctx) (ArgsArity []) `shouldBe` Right ()

    it "accepts one arg (varia)" $ do
        property $ \v -> do
            let varargs = Ident v
            forM_ [none, form] $ \impArgs -> do
                validateNoDuplicateIdents impArgs (ArgsVaria varargs) `shouldBe` Right ()

    it "accepts one arg (arity)" $ do
        property $ \a -> do
            let arg = Ident a
            forM_ [none, form] $ \impArgs -> do
                validateNoDuplicateIdents impArgs (ArgsArity [arg]) `shouldBe` Right ()

    it "accepts two args (impure+varia)" $ do
        property $ \c' v -> do
            let c = differentThan c' [v]
                [ctx, varargs] = map Ident [c, v]
            validateNoDuplicateIdents (both ctx) (ArgsVaria varargs) `shouldBe` Right ()

    it "accepts two args (impure+arity)" $ do
        property $ \c' a -> do
            let c = differentThan c' [a]
                [ctx, arg] = map Ident [c, a]
            validateNoDuplicateIdents (both ctx) (ArgsArity [arg]) `shouldBe` Right ()

    it "accepts two args (arity)" $ do
        property $ \a1' a2' -> do
            let a1 = differentThan a1' []
                a2 = differentThan a2' [a1]
                [arg1, arg2] = map Ident [a1, a2]
            forM_ [none, form] $ \impArgs -> do
                validateNoDuplicateIdents impArgs (ArgsArity [arg1, arg2]) `shouldBe` Right ()

    it "accepts N+1 args (impure+arity)" $ do
        property $ \c' (Uniques as) -> do
            let c = differentThan c' as
                ctx = Ident c
                args = map Ident as
            validateNoDuplicateIdents (both ctx) (ArgsArity args) `shouldBe` Right ()

    it "rejects two args (impure+varia)" $ do
        property $ \x -> do
            let [ctx, varargs] = map Ident [x, x]
            isLeft (validateNoDuplicateIdents (both ctx) (ArgsVaria varargs)) `shouldBe` True

    it "rejects two args (impure+arity)" $ do
        property $ \x -> do
            let [ctx, arg] = map Ident [x, x]
            isLeft (validateNoDuplicateIdents (both ctx) (ArgsArity [arg])) `shouldBe` True

    it "rejects two args (arity)" $ do
        property $ \x -> do
            let [arg1, arg2] = map Ident [x, x]
            forM_ [none, form] $ \impArgs -> do
                isLeft (validateNoDuplicateIdents none (ArgsArity [arg1, arg2])) `shouldBe` True

    it "rejects N+1 args (impure+arity)" $ do
        property $ \x (Uniques as) -> do
            let ctx = Ident x
                args = map Ident (x:as)
            isLeft (validateNoDuplicateIdents (both ctx) (ArgsArity args)) `shouldBe` True

none = None
form = ArgPass $ Eval
both = Both (Eval)

differentThan x xs = if any (==x) xs then '_':concat xs else x