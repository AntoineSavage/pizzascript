module EvalSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as M

import Control.Monad
import Data.Either
import Data.Nat
import Eval
import Quote
import Text.Parsec.Pos  
import TestUtils
import Types
import Utils

spec :: Spec
spec = do
    -- TODO evalExpr vs unevalExpr
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
        property $ \(ArbDict c) p s v -> do
            let sym = symb $ ident s
                k = withPos $ PzSymb sym
                ctx = M.insert k v c

            forM_ [Unquote, DeepUnquote] $ \eval -> do
                evalExpr ctx (WithPos p $ AstSymb sym) eval `shouldBe`
                    Right (Evaled v)

    it "evals identifiers (Eval) as the associated value" $ do
        property $ \(ArbDict c) p s v -> do
            let i = ident s
                k = withPos $ PzSymb $ symb i
                ctx = M.insert k v c

            evalExpr ctx (WithPos p $ AstIdent i) Eval `shouldBe`
                Right (Evaled v)

    it "evals identifiers (Quote) as the corresponding symbol" $ do
        property $ \(ArbDict ctx) p s -> do
            let i = ident s
                sym = symb i
            evalExpr ctx (WithPos p $ AstIdent i) Quote `shouldBe`
                Right (Evaled $ WithPos p $ PzSymb sym)

    it "rejects identifiers (Unquote)" $ do
        property $ \(ArbDict ctx) p s -> do
            let i = ident s
            isLeft (evalExpr ctx (WithPos p $ AstIdent i) Unquote) `shouldBe` True

    it "evals identifiers (DeepQuote) as the associated value, quoted" $ do
        property $ \(ArbDict c) p p2 d s n i' -> do
            let pairs =
                    [ (PzNum d, PzNum d)
                    , (PzStr s, PzStr s)
                    , (PzSymb $ Symb n i', PzSymb $ Symb (S n) i')
                    ]
            forM_ pairs $ \(v, v') -> do
                let i = ident s
                    k = withPos $ PzSymb $ symb i
                    ctx = M.insert k (WithPos p2 v) c

                evalExpr ctx (WithPos p $ AstIdent i) DeepQuote `shouldBe`
                    Right (Evaled $ WithPos p2 v')

    it "evals identifiers (DeepUnquote) as the associated value, unquoted" $ do
        property $ \(ArbDict c) p p2 d s n i' -> do
            let pairs =
                    [ (PzNum d, PzNum d)
                    , (PzStr s, PzStr s)
                    , (PzSymb $ Symb (S n) i', PzSymb $ Symb n i')
                    ]
            forM_ pairs $ \(v', v) -> do
                let i = ident s
                    k = withPos $ PzSymb $ symb i
                    ctx = M.insert k (WithPos p2 v') c

                evalExpr ctx (WithPos p $ AstIdent i) DeepUnquote `shouldBe`
                    Right (Evaled $ WithPos p2 v)

    it "evals lists (Eval) as the associated form" $ do
        property $ \(ArbDict ctx) p k (Few es) -> do
            evalExpr ctx (WithPos p $ AstList k es) Eval `shouldBe`
                Right (ExprForm p $ toForm p k es)

    it "evals lists (Quote and DeepQuote) as themselves, quoted" $ do
        property $ \(ArbDict ctx) p k (Few es) -> do
            forM_ [Quote, DeepQuote] $ \eval -> do
                evalExpr ctx (WithPos p $ AstList k es) eval `shouldBe`
                    evalExpr ctx (quote $ WithPos p $ AstList k es) Eval

    it "evals lists (Unquote and DeepUnquote) as themselves, unquoted" $ do
        property $ \(ArbDict ctx) p k (UnquoteValids es) -> do
            forM_ [Unquote, DeepUnquote] $ \eval -> do
                evalExpr ctx (WithPos p $ AstList k es) eval `shouldBe`
                    (unquote (WithPos p $ AstList k es) >>= \e' -> evalExpr ctx e' Eval)

unevalExprSpec :: Spec
unevalExprSpec = describe "unevalExpr" $ do
    it "unevals unit to empty form" $ do
        property $ \p -> do
            unevalExpr (WithPos p PzUnit) `shouldBe` WithPos p (AstList KindForm [])

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
            unevalExpr (WithPos p $ PzList l) `shouldBe` WithPos p (AstList KindList $ map unevalExpr l)

    it "unevals dict to itself" $ do
        property $ \p (ArbDict d) -> do
            unevalExpr (WithPos p $ PzDict d) `shouldBe`
                WithPos p (AstList KindDict $ flip map (M.assocs d) $ \(k, v) -> withPos $ AstList KindForm [unevalExpr k, unevalExpr v])

    it "unevals built-in function to identifier" $ do
        property $ \p (ArbDict implCtx) impArgs args ident -> do
            unevalExpr (WithPos p $ PzFunc $ Func implCtx impArgs args $ BodyBuiltIn ident) `shouldBe` WithPos p (AstIdent ident)

    it "unevals custom function to list" $ do
        property $ \p (ArbDict implCtx) f@(FuncCustom impArgs args body) -> do
            unevalExpr (WithPos p $ PzFunc $ Func implCtx impArgs args $ BodyCustom body) `shouldBe` WithPos p (AstList KindForm $ unevalFuncCustom f)

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
                , [AstNum 0]
                , [AstStr ""]
                , [AstIdent $ ident ""]
                , [AstSymb $ symb $ ident ""]
                , [AstList KindList [withPos $ AstSymb $ symb $ ident ""]]
                , [AstList KindDict [withPos $ AstSymb $ symb $ ident ""]]
                , [AstList KindForm []]
            ] $ \es -> do
            let elems = map withPos es
            evalImpureArgs elems `shouldBe` Right (None, elems)

    it "evals singleton form to ArgPass" $ do
        property $ \p p2 ap (Few es) -> do
            let elems = (WithPos p $ AstList KindForm [
                        WithPos p2 $ AstSymb $ argPassToSymb ap
                    ]) : es
            evalImpureArgs elems `shouldBe` Right (ArgPass p $ WithPos p2 ap, es)

    it "evals size-2 form to Both" $ do
        property $ \p p2 p3 ap s (Few es) -> do
            let ec = Ident [s]
                elems = (WithPos p $ AstList KindForm [
                        WithPos p2 $ AstSymb $ argPassToSymb ap,
                        WithPos p3 $ AstIdent ec
                    ]) : es
            evalImpureArgs elems `shouldBe` Right (Both p (WithPos p2 ap) (WithPos p3 ec), es)

    it "rejects invalid arg-pass symbol" $ do
        property $ \s (Few es) -> do
            let elems = (withPos $ AstList KindForm [
                        withPos $ AstSymb $ symb $ ident $ '_' : s
                    ]) : es
            isLeft (evalImpureArgs elems) `shouldBe` True

    it "rejects empty identifier" $ do
        property $ \ap (Few es) -> do
            let ec = Ident []
                elems = (withPos $ AstList KindForm [
                        withPos $ AstSymb $ argPassToSymb ap,
                        withPos $ AstIdent ec
                    ]) : es
            isLeft (evalImpureArgs elems) `shouldBe` True

    it "rejects qualified identifier" $ do
        property $ \ap s1 s2 ss (Few es) -> do
            let ec = Ident $ s1:s2:ss
                elems = (withPos $ AstList KindForm [
                        withPos $ AstSymb $ argPassToSymb ap,
                        withPos $ AstIdent ec
                    ]) : es
            isLeft (evalImpureArgs elems) `shouldBe` True

    it "rejects size-3 (or more) form" $ do
        property $ \ap s a (Few as) (Few es) -> do
            let ec = Ident [s]
                elems = (withPos $ AstList KindForm $ [
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
                [WithPos p $ AstList KindForm [
                    WithPos p2 $ AstSymb $ argPassToSymb ap
                ]]

    it "unevals Both to size-2 list" $ do
        property $ \p p2 p3 ap ec -> do
            unevalImpureArgs (Both p (WithPos p2 ap) $ WithPos p3 ec) `shouldBe`
                [WithPos p $ AstList KindForm [
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
    let toAstIdent p s = WithPos p $ AstIdent $ ident s
        toIdent p s = WithPos p $ ident s

    it "evals variadic args ident" $ do
        property $ \p s (Few es) -> do
            let elems = toAstIdent p s : es
            evalArgs elems `shouldBe` Right (ArgsVaria $ toIdent p s, es)

    it "evals arity args idents" $ do
        property $ \p ps (Few es) -> do
            let elems = WithPos p (AstList KindForm $ map (uncurry toAstIdent) ps) : es
            evalArgs elems `shouldBe` Right (ArgsArity p $ map (uncurry toIdent) ps, es)

    it "rejects qualified variadic args ident" $ do
        property $ \s1 s2 (Few es) -> do
            let elems = withPos (AstIdent $ Ident [s1, s2]) : es
            isLeft (evalArgs elems) `shouldBe` True

    it "rejects qualified arity args idents" $ do
        property $ \s1 s2 (Few es) -> do
            let elems = withPos (AstList KindForm [withPos $ AstIdent $ Ident [s1, s2]]) : es
            isLeft (evalArgs elems) `shouldBe` True

    it "rejects empty list" $ do
        isLeft (evalArgs []) `shouldBe` True

    it "rejects non-ident and non-form list" $ do
        property $ \(Few es) -> do
            forM_   [ AstNum 0, AstStr ""
                    , AstSymb $ symb $ ident ""
                    , AstList KindList []
                    , AstList KindDict []
                    ] $ \e ->
                isLeft (evalArgs $ withPos e:es) `shouldBe` True

unevalArgsSpec :: Spec
unevalArgsSpec = describe "unevalArgs" $ do
    it "unevals variadic args ident" $ do
        property $ \i -> do
            unevalArgs (ArgsVaria i) `shouldBe` [fmap AstIdent i]

    it "unevals arity args idents" $ do
        property $ \p is -> do
            unevalArgs (ArgsArity p is) `shouldBe` [WithPos p (AstList KindForm $ flip map is $ fmap AstIdent)]

evalIdentSpec :: Spec
evalIdentSpec = describe "evalIdent" $ do
    it "evaluates zero ident parts" $ do
        property $ \(ArbDict ctx) -> do
            evalIdent ctx (Ident []) `shouldBe` Right (withPos $ PzDict ctx)

    it "evaluates one ident part" $ do
        property $ \(ArbDict c) s v -> do
            let ident = Ident [s]
                k = withPos $ PzSymb $ symb ident
                ctx = flip (M.insert k) c v
            evalIdent ctx ident `shouldBe` Right v

    it "evaluates two ident parts" $ do
        property $ \(ArbDict c1) (ArbDict c2) p1 p2 s1 s2 v -> do
            let k1 = WithPos p1 $ PzSymb $ symb $ Ident [s1]
                k2 = WithPos p2 $ PzSymb $ symb $ Ident [s2]
                ctx2 = flip (M.insert k2) c2 v
                ctx1 = flip (M.insert k1) c1 $ withPos $ PzDict ctx2
            evalIdent ctx1 (Ident [s1, s2]) `shouldBe` Right v

    it "evaluates three ident parts" $ do
        property $ \(ArbDict c1) (ArbDict c2) (ArbDict c3) p1 p2 p3 s1 s2 s3 v -> do
            let k1 = WithPos p1 $ PzSymb $ symb $ Ident [s1]
                k2 = WithPos p2 $ PzSymb $ symb $ Ident [s2]
                k3 = WithPos p3 $ PzSymb $ symb $ Ident [s3]
                ctx3 = flip (M.insert k3) c3 v
                ctx2 = flip (M.insert k2) c2 $ withPos $ PzDict ctx3
                ctx1 = flip (M.insert k1) c1 $ withPos $ PzDict ctx2
            evalIdent ctx1 (Ident [s1, s2, s3]) `shouldBe` Right v

    it "evaluates one undefined ident part" $ do
        property $ \(ArbDict c) s -> do
            let ident = Ident [s]
                k = withPos $ PzSymb $ symb ident
            isLeft (evalIdent (M.delete k c) ident) `shouldBe` True

    it "evaluates two undefined ident parts" $ do
        property $ \(ArbDict c1) (ArbDict c2) p1 p2 s1 s2 -> do
            let ident = Ident [s1, s2]
                k1 = WithPos p1 $ PzSymb $ symb $ Ident [s1]
                k2 = WithPos p2 $ PzSymb $ symb $ Ident [s2]
                ctx = flip (M.insert k1) c1 $ withPos $ PzDict $ M.delete k2 c2
            isLeft (evalIdent ctx ident) `shouldBe` True
            isLeft (evalIdent (M.delete k1 c1) ident) `shouldBe` True

    it "evaluates three undefined ident parts" $ do
        property $ \(ArbDict c1) (ArbDict c2) (ArbDict c3) p1 p2 p3 s1 s2 s3 -> do
            let ident = Ident [s1, s2, s3]
                k1 = WithPos p1 $ PzSymb $ symb $ Ident [s1]
                k2 = WithPos p2 $ PzSymb $ symb $ Ident [s2]
                k3 = WithPos p3 $ PzSymb $ symb $ Ident [s3]
                ctx1 = flip (M.insert k1) c1 $ withPos $ PzDict $
                        flip (M.insert k2) c2 $ withPos $ PzDict $ M.delete k3 c3
                ctx2 = flip (M.insert k1) c1 $ withPos $ PzDict $ M.delete k2 c2
            isLeft (evalIdent ctx1 ident) `shouldBe` True
            isLeft (evalIdent ctx2 ident) `shouldBe` True
            isLeft (evalIdent (M.delete k1 c1) ident) `shouldBe` True

    it "non-dictionary context" $ do
        property $ \(ArbDict c) s1 s2 -> do
            forM_ [PzUnit, PzNum 0, PzStr "", PzList []] $ \v -> do
                let k = withPos $ PzSymb $ symb $ Ident [s1]
                    ctx = flip (M.insert k) c $ withPos v
                isLeft (evalIdent ctx (Ident [s1, s2])) `shouldBe` True

validateNoDuplicateIdentsSpec :: Spec
validateNoDuplicateIdentsSpec = describe "validateNoDuplicateIdents" $ do
    it "accepts zero args" $ do
        property $ \p -> do
            forM_ [none, form] $ \impArgs -> do
                validateNoDuplicateIdents impArgs (ArgsArity p []) `shouldBe` Right ()
                validateNoDuplicateIdents impArgs (ArgsArity p []) `shouldBe` Right ()

    it "accepts one arg (impure args)" $ do
        property $ \p c -> do
            let ctx = withPos $ ident c
            validateNoDuplicateIdents (both ctx) (ArgsArity p []) `shouldBe` Right ()

    it "accepts one arg (varia)" $ do
        property $ \v -> do
            let varargs = withPos $ ident v
            forM_ [none, form] $ \impArgs -> do
                validateNoDuplicateIdents impArgs (ArgsVaria varargs) `shouldBe` Right ()

    it "accepts one arg (arity)" $ do
        property $ \p a -> do
            let arg = withPos $ ident a
            forM_ [none, form] $ \impArgs -> do
                validateNoDuplicateIdents impArgs (ArgsArity p [arg]) `shouldBe` Right ()

    it "accepts two args (impure+varia)" $ do
        property $ \c' v -> do
            let c = differentThan c' [v]
                [ctx, varargs] = map (withPos.ident) [c, v]
            validateNoDuplicateIdents (both ctx) (ArgsVaria varargs) `shouldBe` Right ()

    it "accepts two args (impure+arity)" $ do
        property $ \p c' a -> do
            let c = differentThan c' [a]
                [ctx, arg] = map (withPos.ident) [c, a]
            validateNoDuplicateIdents (both ctx) (ArgsArity p [arg]) `shouldBe` Right ()

    it "accepts two args (arity)" $ do
        property $ \p a1' a2' -> do
            let a1 = differentThan a1' []
                a2 = differentThan a2' [a1]
                [arg1, arg2] = map (withPos.ident) [a1, a2]
            forM_ [none, form] $ \impArgs -> do
                validateNoDuplicateIdents impArgs (ArgsArity p [arg1, arg2]) `shouldBe` Right ()

    it "accepts N+1 args (impure+arity)" $ do
        property $ \p c' (Uniques as) -> do
            let c = differentThan c' as
                ctx = withPos $ ident c
                args = map (withPos.ident) as
            validateNoDuplicateIdents (both ctx) (ArgsArity p args) `shouldBe` Right ()

    it "rejects two args (impure+varia)" $ do
        property $ \x -> do
            let [ctx, varargs] = map (withPos.ident) [x, x]
            isLeft (validateNoDuplicateIdents (both ctx) (ArgsVaria varargs)) `shouldBe` True

    it "rejects two args (impure+arity)" $ do
        property $ \x -> do
            let [ctx, arg] = map (withPos.ident) [x, x]
            isLeft (validateNoDuplicateIdents (both ctx) (ArgsArity p' [arg])) `shouldBe` True

    it "rejects two args (arity)" $ do
        property $ \x -> do
            let [arg1, arg2] = map (withPos.ident) [x, x]
            forM_ [none, form] $ \impArgs -> do
                isLeft (validateNoDuplicateIdents none (ArgsArity p' [arg1, arg2])) `shouldBe` True

    it "rejects N+1 args (impure+arity)" $ do
        property $ \x (Uniques as) -> do
            let ctx = withPos $ ident x
                args = map (withPos.ident) (x:as)
            isLeft (validateNoDuplicateIdents (both ctx) (ArgsArity p' args)) `shouldBe` True

p' = newPos "tests" 0 0
withPos = WithPos p'

none = None
form = ArgPass p' $ withPos Eval
both = Both p' (withPos Eval)

differentThan x xs = if any (==x) xs then '_':concat xs else x