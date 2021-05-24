module EvalSpec where

import Test.Hspec
import Test.QuickCheck

--import qualified Data.Map as M

import Control.Monad
--import Data.AstExpr
import Data.Either
--import Data.Func
--import Data.Func.ArgPass
--import Data.Func.ArgPassSpec
import Data.Func.FuncArgs
--import Data.Func.FuncBody
--import Data.Func.FuncCustom
--import Data.Func.FuncCustomSpec
--import Data.Func.FuncImpureArgs
--import Data.Ident
--import Data.Lst
import Data.Nat
import Data.Numb
--import Data.NumbSpec
import Data.PzVal
import Data.PzValSpec
import Data.Str
--import Data.StrSpec
import Data.Symb
import Data.SymbSpec
import Eval
--import Quote
--import QuoteSpec
--import Text.Parsec.Pos 
import TestUtils
--import Utils

spec :: Spec
spec = do
    evalArgsVsUnevalArgsSpec
    evalArgsSpec
    unevalArgsSpec
    getQuotedIdentSpec

evalArgsVsUnevalArgsSpec :: Spec
evalArgsVsUnevalArgsSpec = describe "evalArgs vs unevalArgs" $ do
    it "composes evalArgs and unevalArgs into id" $ do
        property $ \args (Few es) -> do
            let elems = unevalArgs args
            evalArgs (elems ++ es) `shouldBe` Right (args, es)
            unevalArgs <$> fst <$> evalArgs (elems ++ es) `shouldBe` Right elems

evalArgsSpec :: Spec
evalArgsSpec = describe "evalArgs" $ do
    it "evals variadic args ident" $ do
        property $ \(QuotedIdent s) (Few es) -> do
            let elems = PzSymb s : es
            evalArgs elems `shouldBe` Right (ArgsVaria s, es)

    it "evals arity args idents" $ do
        property $ \(QuotedIdents ss) (Few es) -> do
            let elems = (PzList $ map PzSymb ss) : es
            evalArgs elems `shouldBe` Right (ArgsArity ss, es)

    it "rejects empty list" $ do
        isLeft (evalArgs []) `shouldBe` True

    it "rejects non-ident and non-form list" $ do
        property $ \(Few es) n -> do
            forM_   [ PzNum $ Numb 0, PzStr $ Str ""
                    , PzSymb $ Symb (S n) '_' ""
                    ] $ \e ->
                isLeft (evalArgs $ e:es) `shouldBe` True

unevalArgsSpec :: Spec
unevalArgsSpec = describe "unevalArgs" $ do
    it "unevals variadic args ident" $ do
        property $ \s -> do
            unevalArgs (ArgsVaria s) `shouldBe` [PzSymb s]

    it "unevals arity args idents" $ do
        property $ \ss -> do
            unevalArgs (ArgsArity ss) `shouldBe` [PzList $ map PzSymb ss]

getQuotedIdentSpec :: Spec
getQuotedIdentSpec = describe "getQuotedIdent" $ do
    it "converts quoted ident" $ do
        property $ \(QuotedIdent s) -> do
            getQuotedIdent (PzSymb s) `shouldBe` Right s

    it "rejects number" $ do
        property $ \n ->
            isLeft (getQuotedIdent (PzNum n)) `shouldBe` True

    it "rejects string" $ do
        property $ \s ->
            isLeft (getQuotedIdent (PzStr s)) `shouldBe` True

    it "rejects symbol" $ do
        property $ \s ->
            isLeft (getQuotedIdent (PzSymb s)) `shouldBe` True

    it "rejects list" $ do
        property $ \l ->
            isLeft (getQuotedIdent (PzList l)) `shouldBe` True