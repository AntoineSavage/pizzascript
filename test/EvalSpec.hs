module EvalSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as M

import Control.Monad
import Data.ArgPass
import Data.ArgPassSpec
import Data.AstExpr
import Data.Either
import Data.FuncArgs
import Data.FuncBody
import Data.FuncCustom
import Data.FuncCustomSpec
import Data.FuncImpureArgs
import Data.Ident
import Data.Lst
import Data.Nat
import Data.Numb
import Data.NumbSpec
import Data.Str
import Data.StrSpec
import Data.Symb
import Data.SymbSpec
import Data.WithPos
import Eval
import Quote
import QuoteSpec
import Text.Parsec.Pos 
import TestUtils
import TestUtils2
import Types
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
        property $ \(ArbDict ctx) p d -> do
            forM_ argPasses $ \eval -> do
                let v = WithPos p $ PzNum d
                evalExpr ctx (unevalExpr v) eval `shouldBe` Right (Evaled v)

    it "composes evalExpr and unevalExpr into id (strings)" $ do
        property $ \(ArbDict ctx) p s -> do
            forM_ argPasses $ \eval -> do
                let v = WithPos p $ PzStr s
                evalExpr ctx (unevalExpr v) eval `shouldBe` Right (Evaled v)

    it "composes evalExpr and unevalExpr into id (symbols, Eval)" $ do
        property $ \(ArbDict ctx) p s -> do
            let v = WithPos p $ PzSymb s
            evalExpr ctx (unevalExpr v) Eval `shouldBe` Right (Evaled v)

    -- TODO: Unit
    -- TODO identifiers
    -- TODO symbols (except Eval)
    -- TODO lists

evalExprSpec :: Spec
evalExprSpec = describe "evalExpr" $ do
    it "evals numbers to themselves" $ do
        property $ \(ArbDict ctx) p d -> do
            forM_ argPasses $ \eval -> do
                evalExpr ctx (WithPos p $ AstNum d) eval `shouldBe`
                    Right (Evaled $ WithPos p $ PzNum d)

    it "evals strings to themselves" $ do
        property $ \(ArbDict ctx) p s -> do
            forM_ argPasses $ \eval -> do
                evalExpr ctx (WithPos p $ AstStr s) eval `shouldBe`
                    Right (Evaled $ WithPos p $ PzStr s)

    it "evals symbols (Eval) to themselves" $ do
        property $ \(ArbDict ctx) p s -> do
            evalExpr ctx (WithPos p $ AstSymb s) Eval `shouldBe`
                Right (Evaled $ WithPos p $ PzSymb s)

    it "evals symbols (Quote or DeepQuote) as themselves with one more quote" $ do
        property $ \(ArbDict ctx) p n i -> do
            forM_ [Quote, DeepQuote] $ \eval -> do
                evalExpr ctx (WithPos p $ AstSymb $ Symb n i) eval `shouldBe`
                    Right (Evaled $ WithPos p $ PzSymb $ Symb (S n) i)

    it "evals symbols (Unquote or DeepUnquote) as themselves with one less quote" $ do
        property $ \(ArbDict ctx) p n i -> do
            forM_ [Unquote, DeepUnquote] $ \eval -> do
                evalExpr ctx (WithPos p $ AstSymb $ Symb (S n) i) eval `shouldBe`
                    Right (Evaled $ WithPos p $ PzSymb $ Symb n i)

    it "evals single-quoted symbols (Unquote or DeepUnquote) as the matching identifier, evaluated" $ do
        property $ \(ArbDict c) p i v -> do
            let sym = symb i
                k = withPos $ PzSymb sym
                ctx = M.insert k v c

            forM_ [Unquote, DeepUnquote] $ \eval -> do
                evalExpr ctx (WithPos p $ AstSymb sym) eval `shouldBe`
                    Right (Evaled v)

    it "evals identifiers (Eval) as the associated value" $ do
        property $ \(ArbDict c) p i v -> do
            let k = withPos $ PzSymb $ symb i
                ctx = M.insert k v c

            evalExpr ctx (WithPos p $ AstIdent i) Eval `shouldBe`
                Right (Evaled v)

    it "evals identifiers (Quote) as the corresponding symbol" $ do
        property $ \(ArbDict ctx) p i -> do
            let sym = symb i
            evalExpr ctx (WithPos p $ AstIdent i) Quote `shouldBe`
                Right (Evaled $ WithPos p $ PzSymb sym)

    it "rejects identifiers (Unquote)" $ do
        property $ \(ArbDict ctx) p i -> do
            isLeft (evalExpr ctx (WithPos p $ AstIdent i) Unquote) `shouldBe` True

    it "evals identifiers (DeepQuote) as the associated value, quoted" $ do
        property $ \(ArbDict c) p p2 d s1 s2 n i' -> do
            let pairs =
                    [ (PzNum d, PzNum d)
                    , (PzStr s1, PzStr s1)
                    , (PzSymb $ Symb n i', PzSymb $ Symb (S n) i')
                    ]
            forM_ pairs $ \(v, v') -> do
                let i = Ident s2
                    k = withPos $ PzSymb $ symb i
                    ctx = M.insert k (WithPos p2 v) c

                evalExpr ctx (WithPos p $ AstIdent i) DeepQuote `shouldBe`
                    Right (Evaled $ WithPos p2 v')

    it "evals identifiers (DeepUnquote) as the associated value, unquoted" $ do
        property $ \(ArbDict c) p p2 d s1 s2 n i' -> do
            let pairs =
                    [ (PzNum d, PzNum d)
                    , (PzStr s1, PzStr s1)
                    , (PzSymb $ Symb (S n) i', PzSymb $ Symb n i')
                    ]
            forM_ pairs $ \(v', v) -> do
                let i = Ident s2
                    k = withPos $ PzSymb $ symb i
                    ctx = M.insert k (WithPos p2 v') c

                evalExpr ctx (WithPos p $ AstIdent i) DeepUnquote `shouldBe`
                    Right (Evaled $ WithPos p2 v)

    it "evals lists (Eval) as the associated form" $ do
        property $ \(ArbDict ctx) p k (Few es) -> do
            evalExpr ctx (WithPos p $ AstList $ Lst k es) Eval `shouldBe`
                Right (ExprForm p $ toForm p k es)

    it "evals lists (Quote and DeepQuote) as themselves, quoted" $ do
        property $ \(ArbDict ctx) p k (Few es) -> do
            forM_ [Quote, DeepQuote] $ \eval -> do
                evalExpr ctx (WithPos p $ AstList $ Lst k es) eval `shouldBe`
                    evalExpr ctx (quote $ WithPos p $ AstList $ Lst k es) Eval

    it "evals lists (Unquote and DeepUnquote) as themselves, unquoted" $ do
        property $ \(ArbDict ctx) p k (UnquoteValids es) -> do
            forM_ [Unquote, DeepUnquote] $ \eval -> do
                evalExpr ctx (WithPos p $ AstList $ Lst k es) eval `shouldBe`
                    (unquote (WithPos p $ AstList $ Lst k es) >>= \e' -> evalExpr ctx e' Eval)

unevalExprSpec :: Spec
unevalExprSpec = describe "unevalExpr" $ do
    it "unevals unit to empty form" $ do
        property $ \p -> do
            unevalExpr (WithPos p PzUnit) `shouldBe` WithPos p (AstList $ Lst KindForm [])

    it "unevals number to itself" $ do
        property $ \p d -> do
            unevalExpr (WithPos p $ PzNum d) `shouldBe` WithPos p (AstNum d)

    it "unevals string to itself" $ do
        property $ \p s -> do
            unevalExpr (WithPos p $ PzStr s) `shouldBe` WithPos p (AstStr s)

    it "unevals symbol to itself" $ do
        property $ \p s -> do
            unevalExpr (WithPos p $ PzSymb s) `shouldBe` WithPos p (AstSymb s)

    it "unevals list to itself" $ do
        property $ \p (Few l) -> do
            unevalExpr (WithPos p $ PzList l) `shouldBe` WithPos p (AstList $ Lst KindList $ map unevalExpr l)

    it "unevals dict to itself" $ do
        property $ \p (ArbDict d) -> do
            unevalExpr (WithPos p $ PzDict d) `shouldBe`
                WithPos p (AstList $ Lst KindDict $ flip map (M.assocs d) $ \(k, v) -> withPos $ AstList $ Lst KindForm [unevalExpr k, unevalExpr v])

    it "unevals built-in function to identifier" $ do
        property $ \p (ArbDict implCtx) impArgs args ident -> do
            unevalExpr (WithPos p $ PzFunc $ Func implCtx impArgs args $ BodyBuiltIn ident) `shouldBe` WithPos p (AstIdent ident)

    it "unevals custom function to list" $ do
        property $ \p (ArbDict implCtx) f@(FuncCustom impArgs args body) -> do
            unevalExpr (WithPos p $ PzFunc $ Func implCtx impArgs args $ BodyCustom body) `shouldBe` WithPos p (AstList $ Lst KindForm $ unevalFuncCustom f)

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
                , [AstList $ Lst KindList [withPos $ AstSymb $ symb $ Ident ""]]
                , [AstList $ Lst KindDict [withPos $ AstSymb $ symb $ Ident ""]]
                , [AstList $ Lst KindForm []]
            ] $ \es -> do
            let elems = map withPos es
            evalImpureArgs elems `shouldBe` Right (None, elems)

    it "evals singleton form to ArgPass" $ do
        property $ \p p2 ap (Few es) -> do
            let elems = (WithPos p $ AstList $ Lst KindForm [
                        WithPos p2 $ AstSymb $ argPassToSymb ap
                    ]) : es
            evalImpureArgs elems `shouldBe` Right (ArgPass p $ WithPos p2 ap, es)

    it "evals size-2 form to Both" $ do
        property $ \p p2 p3 ap s (Few es) -> do
            let ec = Ident [s]
                elems = (WithPos p $ AstList $ Lst KindForm [
                        WithPos p2 $ AstSymb $ argPassToSymb ap,
                        WithPos p3 $ AstIdent ec
                    ]) : es
            evalImpureArgs elems `shouldBe` Right (Both p (WithPos p2 ap) (WithPos p3 ec), es)

    it "rejects invalid arg-pass symbol" $ do
        property $ \s (Few es) -> do
            let elems = (withPos $ AstList $ Lst KindForm [
                        withPos $ AstSymb $ symb $ Ident $ '_' : s
                    ]) : es
            isLeft (evalImpureArgs elems) `shouldBe` True

    it "rejects size-3 (or more) form" $ do
        property $ \ap ec a (Few as) (Few es) -> do
            let elems = (withPos $ AstList $ Lst KindForm $ [
                        withPos $ AstSymb $ argPassToSymb ap,
                        withPos $ AstIdent ec
                    ] ++ [a] ++ as) : es
            isLeft (evalImpureArgs elems) `shouldBe` True

unevalImpureArgsSpec :: Spec
unevalImpureArgsSpec = describe "unevalImpureArgs" $ do
    it "unevals None to empty list" $ do
        unevalImpureArgs None `shouldBe` []

    it "unevals ArgPass to singleton list" $ do
        property $ \p p2 ap -> do
            unevalImpureArgs (ArgPass p $ WithPos p2 ap) `shouldBe`
                [WithPos p $ AstList $ Lst KindForm [
                    WithPos p2 $ AstSymb $ argPassToSymb ap
                ]]

    it "unevals Both to size-2 list" $ do
        property $ \p p2 p3 ap ec -> do
            unevalImpureArgs (Both p (WithPos p2 ap) $ WithPos p3 ec) `shouldBe`
                [WithPos p $ AstList $ Lst KindForm [
                    WithPos p2 $ AstSymb $ argPassToSymb ap,
                    WithPos p3 $ AstIdent ec
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
    let toAstIdent p s = WithPos p $ AstIdent $ Ident s
        toIdent p s = WithPos p $ Ident s

    it "evals variadic args ident" $ do
        property $ \p s (Few es) -> do
            let elems = toAstIdent p s : es
            evalArgs elems `shouldBe` Right (ArgsVaria $ toIdent p s, es)

    it "evals arity args idents" $ do
        property $ \p ps (Few es) -> do
            let elems = WithPos p (AstList $ Lst KindForm $ map (uncurry toAstIdent) ps) : es
            evalArgs elems `shouldBe` Right (ArgsArity p $ map (uncurry toIdent) ps, es)

    it "rejects empty list" $ do
        isLeft (evalArgs []) `shouldBe` True

    it "rejects non-ident and non-form list" $ do
        property $ \(Few es) -> do
            forM_   [ AstNum $ Numb 0, AstStr $ Str ""
                    , AstSymb $ symb $ Ident ""
                    , AstList $ Lst KindList []
                    , AstList $ Lst KindDict []
                    ] $ \e ->
                isLeft (evalArgs $ withPos e:es) `shouldBe` True

unevalArgsSpec :: Spec
unevalArgsSpec = describe "unevalArgs" $ do
    it "unevals variadic args ident" $ do
        property $ \i -> do
            unevalArgs (ArgsVaria i) `shouldBe` [fmap AstIdent i]

    it "unevals arity args idents" $ do
        property $ \p is -> do
            unevalArgs (ArgsArity p is) `shouldBe` [WithPos p (AstList $ Lst KindForm $ flip map is $ fmap AstIdent)]

evalIdentSpec :: Spec
evalIdentSpec = describe "evalIdent" $ do
    it "evaluates one ident part" $ do
        property $ \(ArbDict c) ident v -> do
            let k = withPos $ PzSymb $ symb ident
                ctx = flip (M.insert k) c v
            evalIdent ctx ident `shouldBe` Right v

    it "evaluates undefined ident" $ do
        property $ \(ArbDict c) ident -> do
            let k = withPos $ PzSymb $ symb ident
            isLeft (evalIdent (M.delete k c) ident) `shouldBe` True

validateNoDuplicateIdentsSpec :: Spec
validateNoDuplicateIdentsSpec = describe "validateNoDuplicateIdents" $ do
    it "accepts zero args" $ do
        property $ \p -> do
            forM_ [none, form] $ \impArgs -> do
                validateNoDuplicateIdents impArgs (ArgsArity p []) `shouldBe` Right ()
                validateNoDuplicateIdents impArgs (ArgsArity p []) `shouldBe` Right ()

    it "accepts one arg (impure args)" $ do
        property $ \p c -> do
            let ctx = withPos $ Ident c
            validateNoDuplicateIdents (both ctx) (ArgsArity p []) `shouldBe` Right ()

    it "accepts one arg (varia)" $ do
        property $ \v -> do
            let varargs = withPos $ Ident v
            forM_ [none, form] $ \impArgs -> do
                validateNoDuplicateIdents impArgs (ArgsVaria varargs) `shouldBe` Right ()

    it "accepts one arg (arity)" $ do
        property $ \p a -> do
            let arg = withPos $ Ident a
            forM_ [none, form] $ \impArgs -> do
                validateNoDuplicateIdents impArgs (ArgsArity p [arg]) `shouldBe` Right ()

    it "accepts two args (impure+varia)" $ do
        property $ \c' v -> do
            let c = differentThan c' [v]
                [ctx, varargs] = map (withPos.Ident) [c, v]
            validateNoDuplicateIdents (both ctx) (ArgsVaria varargs) `shouldBe` Right ()

    it "accepts two args (impure+arity)" $ do
        property $ \p c' a -> do
            let c = differentThan c' [a]
                [ctx, arg] = map (withPos.Ident) [c, a]
            validateNoDuplicateIdents (both ctx) (ArgsArity p [arg]) `shouldBe` Right ()

    it "accepts two args (arity)" $ do
        property $ \p a1' a2' -> do
            let a1 = differentThan a1' []
                a2 = differentThan a2' [a1]
                [arg1, arg2] = map (withPos.Ident) [a1, a2]
            forM_ [none, form] $ \impArgs -> do
                validateNoDuplicateIdents impArgs (ArgsArity p [arg1, arg2]) `shouldBe` Right ()

    it "accepts N+1 args (impure+arity)" $ do
        property $ \p c' (Uniques as) -> do
            let c = differentThan c' as
                ctx = withPos $ Ident c
                args = map (withPos.Ident) as
            validateNoDuplicateIdents (both ctx) (ArgsArity p args) `shouldBe` Right ()

    it "rejects two args (impure+varia)" $ do
        property $ \x -> do
            let [ctx, varargs] = map (withPos.Ident) [x, x]
            isLeft (validateNoDuplicateIdents (both ctx) (ArgsVaria varargs)) `shouldBe` True

    it "rejects two args (impure+arity)" $ do
        property $ \x -> do
            let [ctx, arg] = map (withPos.Ident) [x, x]
            isLeft (validateNoDuplicateIdents (both ctx) (ArgsArity p' [arg])) `shouldBe` True

    it "rejects two args (arity)" $ do
        property $ \x -> do
            let [arg1, arg2] = map (withPos.Ident) [x, x]
            forM_ [none, form] $ \impArgs -> do
                isLeft (validateNoDuplicateIdents none (ArgsArity p' [arg1, arg2])) `shouldBe` True

    it "rejects N+1 args (impure+arity)" $ do
        property $ \x (Uniques as) -> do
            let ctx = withPos $ Ident x
                args = map (withPos.Ident) (x:as)
            isLeft (validateNoDuplicateIdents (both ctx) (ArgsArity p' args)) `shouldBe` True

p' = newPos "tests" 0 0
withPos = WithPos p'

none = None
form = ArgPass p' $ withPos Eval
both = Both p' (withPos Eval)

differentThan x xs = if any (==x) xs then '_':concat xs else x