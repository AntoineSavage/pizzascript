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
    evalIdentSpec
    validateNoDuplicateIdentsSpec

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
        forM_ [none, form] $ \impArgs -> do
            validateNoDuplicateIdents impArgs (ArgsArity []) `shouldBe` Right ()
            validateNoDuplicateIdents impArgs (ArgsArity []) `shouldBe` Right ()

    it "accepts one arg (impure args)" $ do
        property $ \c -> do
            let ctx = withPos $ ident c
            validateNoDuplicateIdents (both ctx) (ArgsArity []) `shouldBe` Right ()

    it "accepts one arg (varia)" $ do
        property $ \v -> do
            let varargs = withPos $ ident v
            forM_ [none, form] $ \impArgs -> do
                validateNoDuplicateIdents impArgs (ArgsVaria varargs) `shouldBe` Right ()

    it "accepts one arg (arity)" $ do
        property $ \a -> do
            let arg = withPos $ ident a
            forM_ [none, form] $ \impArgs -> do
                validateNoDuplicateIdents impArgs (ArgsArity [arg]) `shouldBe` Right ()

    it "accepts two args (impure+varia)" $ do
        property $ \c' v -> do
            let c = differentThan c' [v]
                [ctx, varargs] = map (withPos.ident) [c, v]
            validateNoDuplicateIdents (both ctx) (ArgsVaria varargs) `shouldBe` Right ()

    it "accepts two args (impure+arity)" $ do
        property $ \c' a -> do
            let c = differentThan c' [a]
                [ctx, arg] = map (withPos.ident) [c, a]
            validateNoDuplicateIdents (both ctx) (ArgsArity [arg]) `shouldBe` Right ()

    it "accepts two args (arity)" $ do
        property $ \a1' a2' -> do
            let a1 = differentThan a1' []
                a2 = differentThan a2' [a1]
                [arg1, arg2] = map (withPos.ident) [a1, a2]
            forM_ [none, form] $ \impArgs -> do
                validateNoDuplicateIdents impArgs (ArgsArity [arg1, arg2]) `shouldBe` Right ()

    it "accepts N+1 args (impure+arity)" $ do
        property $ \c' (Uniques as) -> do
            let c = differentThan c' as
                ctx = withPos $ ident c
                args = map (withPos.ident) as
            validateNoDuplicateIdents (both ctx) (ArgsArity args) `shouldBe` Right ()

    it "rejects two args (impure+varia)" $ do
        property $ \x -> do
            let [ctx, varargs] = map (withPos.ident) [x, x]
            isLeft (validateNoDuplicateIdents (both ctx) (ArgsVaria varargs)) `shouldBe` True

    it "rejects two args (impure+arity)" $ do
        property $ \x -> do
            let [ctx, arg] = map (withPos.ident) [x, x]
            isLeft (validateNoDuplicateIdents (both ctx) (ArgsArity [arg])) `shouldBe` True

    it "rejects two args (arity)" $ do
        property $ \x -> do
            let [arg1, arg2] = map (withPos.ident) [x, x]
            forM_ [none, form] $ \impArgs -> do
                isLeft (validateNoDuplicateIdents none (ArgsArity [arg1, arg2])) `shouldBe` True

    it "rejects N+1 args (impure+arity)" $ do
        property $ \x (Uniques as) -> do
            let ctx = withPos $ ident x
                args = map (withPos.ident) (x:as)
            isLeft (validateNoDuplicateIdents (both ctx) (ArgsArity args)) `shouldBe` True

p' = newPos "tests" 0 0
withPos = WithPos p'

none = None
form = ArgPass p' $ withPos Eval
both = Both p' (withPos Eval)

differentThan x xs = if any (==x) xs then '_':concat xs else x