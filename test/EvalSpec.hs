module EvalSpec where

import Test.Hspec
import Test.QuickCheck

--import qualified Data.Map as M

import Control.Monad
--import Data.AstExpr
import Data.Either
--import Data.Func
import Data.Func.ArgPass
import Data.Func.ArgPassSpec
import Data.Func.FuncArgs
--import Data.Func.FuncBody
--import Data.Func.FuncCustom
--import Data.Func.FuncCustomSpec
import Data.Func.FuncImpureArgs
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
    evalImpureArgsVsUnevalImpureArgsSpec
    evalImpureArgsSpec
    unevalImpureArgsSpec
    evalArgsVsUnevalArgsSpec
    evalArgsSpec
    unevalArgsSpec
    getQuotedIdentSpec

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
                , [PzNum $ Numb 0]
                , [PzStr $ Str ""]
                , [PzSymb $ symb "_"]
                , [PzList $ []]
                , [PzList $ [PzNum $ Numb 0]]
                , [PzList $ [PzStr $ Str ""]]
                , [PzList $ [PzList []]]
            ] $ \es -> do
            evalImpureArgs es `shouldBe` Right (None, es)

    it "evals singleton form to ArgPass" $ do
        property $ \ap (Few es) -> do
            let elems = PzList [PzSymb $ argPassToSymb ap] : es
            evalImpureArgs elems `shouldBe` Right (ArgPass ap, es)

    it "evals size-2 form to Both" $ do
        property $ \ap (QuotedIdent ec) (Few es) -> do
            let elems = PzList [PzSymb $ argPassToSymb ap, PzSymb ec] : es
            evalImpureArgs elems `shouldBe` Right (Both ap ec, es)

    it "rejects invalid arg-pass symbol" $ do
        property $ \s (Few es) -> do
            let elems = PzList [PzSymb $ symb $ '_' : s] : es
            isLeft (evalImpureArgs elems) `shouldBe` True

    it "rejects size-3 (or more) form" $ do
        property $ \ap (QuotedIdent ec) a (Few as) (Few es) -> do
            let elems = (PzList $ [ PzSymb $ argPassToSymb ap, PzSymb ec] ++ [a] ++ as) : es
            isLeft (evalImpureArgs elems) `shouldBe` True


unevalImpureArgsSpec :: Spec
unevalImpureArgsSpec = describe "unevalImpureArgs" $ do
    it "unevals None to empty list" $ do
        unevalImpureArgs None `shouldBe` []

    it "unevals ArgPass to singleton list" $ do
        property $ \ap -> do
            unevalImpureArgs (ArgPass ap) `shouldBe` [PzList [PzSymb $ argPassToSymb ap]]

    it "unevals Both to size-2 list" $ do
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