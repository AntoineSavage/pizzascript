module EvalSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as M

import Control.Monad
import Data.Either
import Eval
import Text.Parsec.Pos  
import TestUtils
import Types
import Utils

spec :: Spec
spec = do
    evalImpureArgsVsUnevalImpureArgsSpec
    evalImpureArgsSpec
    unevalImpureArgsSpec
    evalArgsVsUnevalArgsSpec
    evalArgsSpec
    unevalArgsSpec
    evalIdentSpec
    validateNoDuplicateIdentsSpec

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
        property $ \(ArbDict ctx) p -> do
            evalIdent ctx p (Ident []) `shouldBe` Right (WithPos p $ PzDict ctx)

    it "evaluates one ident part" $ do
        property $ \(ArbDict c) p s v -> do
            let ident = Ident [s]
                k = WithPos p $ PzSymb $ symb ident
                ctx = flip (M.insert k) c v
            evalIdent ctx p' ident `shouldBe` Right v

    it "evaluates two ident parts" $ do
        property $ \(ArbDict c1) (ArbDict c2) p1 p2 s1 s2 v -> do
            let k1 = WithPos p1 $ PzSymb $ symb $ Ident [s1]
                k2 = WithPos p2 $ PzSymb $ symb $ Ident [s2]
                ctx2 = flip (M.insert k2) c2 v
                ctx1 = flip (M.insert k1) c1 $ withPos $ PzDict ctx2
            evalIdent ctx1 p' (Ident [s1, s2]) `shouldBe` Right v

    it "evaluates three ident parts" $ do
        property $ \(ArbDict c1) (ArbDict c2) (ArbDict c3) p1 p2 p3 s1 s2 s3 v -> do
            let k1 = WithPos p1 $ PzSymb $ symb $ Ident [s1]
                k2 = WithPos p2 $ PzSymb $ symb $ Ident [s2]
                k3 = WithPos p3 $ PzSymb $ symb $ Ident [s3]
                ctx3 = flip (M.insert k3) c3 v
                ctx2 = flip (M.insert k2) c2 $ withPos $ PzDict ctx3
                ctx1 = flip (M.insert k1) c1 $ withPos $ PzDict ctx2
            evalIdent ctx1 p' (Ident [s1, s2, s3]) `shouldBe` Right v

    it "evaluates one undefined ident part" $ do
        property $ \(ArbDict c) p s -> do
            let ident = Ident [s]
                k = WithPos p $ PzSymb $ symb ident
            isLeft (evalIdent (M.delete k c) p' ident) `shouldBe` True

    it "evaluates two undefined ident parts" $ do
        property $ \(ArbDict c1) (ArbDict c2) p1 p2 s1 s2 -> do
            let ident = Ident [s1, s2]
                k1 = WithPos p1 $ PzSymb $ symb $ Ident [s1]
                k2 = WithPos p2 $ PzSymb $ symb $ Ident [s2]
                ctx = flip (M.insert k1) c1 $ withPos $ PzDict $ M.delete k2 c2
            isLeft (evalIdent ctx p' ident) `shouldBe` True
            isLeft (evalIdent (M.delete k1 c1) p' ident) `shouldBe` True

    it "evaluates three undefined ident parts" $ do
        property $ \(ArbDict c1) (ArbDict c2) (ArbDict c3) p1 p2 p3 s1 s2 s3 -> do
            let ident = Ident [s1, s2, s3]
                k1 = WithPos p1 $ PzSymb $ symb $ Ident [s1]
                k2 = WithPos p2 $ PzSymb $ symb $ Ident [s2]
                k3 = WithPos p3 $ PzSymb $ symb $ Ident [s3]
                ctx1 = flip (M.insert k1) c1 $ withPos $ PzDict $
                        flip (M.insert k2) c2 $ withPos $ PzDict $ M.delete k3 c3
                ctx2 = flip (M.insert k1) c1 $ withPos $ PzDict $ M.delete k2 c2
            isLeft (evalIdent ctx1 p' ident) `shouldBe` True
            isLeft (evalIdent ctx2 p' ident) `shouldBe` True
            isLeft (evalIdent (M.delete k1 c1) p' ident) `shouldBe` True

    it "non-dictionary context" $ do
        property $ \(ArbDict c) p s1 s2 -> do
            forM_ [PzUnit, PzNum 0, PzStr "", PzList []] $ \v -> do
                let k = WithPos p $ PzSymb $ symb $ Ident [s1]
                    ctx = flip (M.insert k) c $ withPos v
                isLeft (evalIdent ctx p' (Ident [s1, s2])) `shouldBe` True

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