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
    evalFuncCustomSpec
    unevalFuncCustomSpec
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

-- QuoteSpec
module QuoteSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.AstExpr
import Data.AstExprSpec
import Data.Either
import Data.Ident
import Data.IdentSpec
import Data.Lst
import Data.Nat
import Data.NatSpec
import Data.Numb
import Data.Str
import Data.Symb
import Quote
import TestUtils
import Utils

spec :: Spec
spec = do
    quoteVsUnquoteSpec
    quoteSpec
    unquoteSpec

quoteVsUnquoteSpec :: Spec
quoteVsUnquoteSpec = describe "quote vs unquote" $ do
    it "composes quote and unquote into id" $ do
        property $ \e1 (UnquoteValid e2) -> do
            quote <$> unquote (quote e1) `shouldBe` Right (quote e1)
            quote <$> unquote e2 `shouldBe` Right e2

quoteSpec :: Spec
quoteSpec = describe "quote" $ do
    it "converts numbers into themselves" $ do
        property $ \n -> do
            let e = AstNum $ Numb n
            quote e `shouldBe` e

    it "converts strings into themselves" $ do
        property $ \s -> do
            let e = AstStr $ Str s
            quote e `shouldBe` e

    it "converts identifiers into single-quoted symbols" $ do
        property $ \i -> do
            quote (AstIdent i) `shouldBe` AstSymb (Symb Z i)

    it "converts symbols into one-more-quoted symbols" $ do
        property $ \n ident -> do
            quote (AstSymb $ Symb n ident) `shouldBe` AstSymb (Symb (S n) ident)

    it "converts lists into 'list-prefixed lists" $ do
        property $ \(Few es) -> do
            quote (AstList $ Lst KindList es) `shouldBe`
                AstList (Lst KindList $ map quote $ toForm KindList es)

    it "converts dicts into 'dict-prefixed lists" $ do
        property $ \(Few es) -> do
            quote (AstList $ Lst KindDict es) `shouldBe`
                AstList (Lst KindList $ map quote $ toForm KindDict es)

    it "converts forms into lists" $ do
        property $ \(Few es) -> do
            quote (AstList $ Lst KindForm es) `shouldBe`
                AstList (Lst KindList $ map quote es)

unquoteSpec :: Spec
unquoteSpec = describe "unquote" $ do
    it "converts numbers into themselves" $ do
        property $ \n -> do
            let e = AstNum $ Numb n
            unquote e `shouldBe` Right e

    it "converts strings into themselves" $ do
        property $ \s -> do
            let e = AstStr $ Str s
            unquote e `shouldBe` Right e

    it "rejects identifiers" $ do
        property $ \i -> do
            let e = AstIdent i
            unquote e `shouldBe` Left ("Unquote: unexpected identifier: " ++ unparseIdent i)

    it "converts single-quoted symbols into identifiers" $ do
        property $ \i -> do
            unquote (AstSymb $ Symb Z i) `shouldBe` Right (AstIdent i)

    it "converts two-or-more-quoted symbols into one-less-quoted symbol" $ do
        property $ \n i -> do
            unquote (AstSymb $ Symb (S n) i) `shouldBe` Right (AstSymb $ Symb n i)

    it "converts lists into forms" $ do
        property $ \(UnquoteValids es) -> do
            let list = AstList $ Lst KindList es
                mactual = unquote list
            isRight mactual `shouldBe` True
            mactual `shouldBe` (AstList . Lst KindForm <$> mapM unquote es)
   
    it "rejects dictionaries" $ do
        property $ \(Few es) -> do
            let dictionary = AstList $ Lst KindDict es
            unquote dictionary `shouldBe`
                Left ("Unquote: unexpected dictionary: " ++ unparseLst unparse (Lst KindDict es))

    it "rejects forms" $ do
        property $ \(Few es) -> do
            let form = AstList $ Lst KindForm es
            unquote form `shouldBe`
                Left ("Unquote: unexpected form: " ++ unparseLst unparse (Lst KindForm es))

-- Utils
arbUnquoteValid :: Gen AstExpr
arbUnquoteValid = do UnquoteValid e <- arbitrary; return e

newtype UnquoteValid = UnquoteValid AstExpr deriving (Show, Eq)
instance Arbitrary UnquoteValid where arbitrary = arbDepth
instance ArbWithDepth UnquoteValid where
    arbWithDepth depth = fmap UnquoteValid $ oneof $
        [ AstNum . Numb <$> arbitrary
        , AstStr . Str <$> arbitrary
        , AstSymb <$> liftM2 Symb arbitrary arbitrary
        ] ++
        ( if depth <= 0 then [] else
            [ (AstList . Lst KindList) <$> arbFew arbUnquoteValid
            ]
        )

newtype UnquoteValids = UnquoteValids [AstExpr] deriving (Show, Eq)
instance Arbitrary UnquoteValids where arbitrary = UnquoteValids <$> arbFew arbUnquoteValid