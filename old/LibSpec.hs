-- EvalSpec
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
    evalIdentSpec

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