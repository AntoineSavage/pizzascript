module EvalSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Either
import Eval
import Text.Parsec.Pos
import TestUtils
import Types
import Utils

spec :: Spec
spec = do
    validateNoDuplicateIdentsSpec

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

p = newPos "tests" 0 0
withPos = WithPos p

none = None
form = ArgPass p $ withPos Eval
both = Both p (withPos Eval)

differentThan x xs = if any (==x) xs then '_':concat xs else x