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
import Data.Func.FuncCustom
import Data.Func.FuncCustomSpec
import Data.Func.FuncImpureArgs
--import Data.Ident
import Data.List
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
    evalFuncCustomVsUnevalFuncCustomSpec
    evalFuncCustomSpec
    unevalFuncCustomSpec
    evalImpureArgsVsUnevalImpureArgsSpec
    evalImpureArgsSpec
    unevalImpureArgsSpec
    evalArgsVsUnevalArgsSpec
    evalArgsSpec
    unevalArgsSpec
    validateNoDuplicateQuotedIdentsSpec
    unconsFuncBodySpec
    getQuotedIdentSpec

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
        property $ \func@(FuncCustom impArgs args x xs) -> do
            let elems = unevalImpureArgs impArgs ++ unevalArgs args ++ [x] ++ xs
            evalFuncCustom elems `shouldBe` Right func

    it "rejects empty elems" $ do
        isLeft (evalFuncCustom []) `shouldBe` True

unevalFuncCustomSpec :: Spec
unevalFuncCustomSpec = describe "unevalFuncCustom" $ do
    it "unevals custom function" $ do
        property $ \func@(FuncCustom impArgs args x xs) -> do
            unevalFuncCustom func `shouldBe` unevalImpureArgs impArgs ++ unevalArgs args ++ [x] ++ xs

evalImpureArgsVsUnevalImpureArgsSpec :: Spec
evalImpureArgsVsUnevalImpureArgsSpec = describe "evalImpureArgs vs unevalImpureArgs" $ do
    it "composes evalImpureArgs and unevalImpureArgs into id" $ do
        property $ \impArgs n (Few es') -> do
            let es = PzNum n : es'
                elems = unevalImpureArgs impArgs
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
                , [PzList $ [PzSymb $ symb "_"]]
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
            let elems = PzList [PzSymb $ Symb (S Z) '_' s] : es
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
        property $ \(Few ss) -> do
            unevalArgs (ArgsArity ss) `shouldBe` [PzList $ map PzSymb ss]


validateNoDuplicateQuotedIdentsSpec :: Spec
validateNoDuplicateQuotedIdentsSpec = describe "validateNoDuplicateQuotedIdents" $ do
    it "accepts zero args" $ do
        forM_ [none, form] $ \impArgs -> do
            validateNoDuplicateQuotedIdents impArgs (ArgsArity []) `shouldBe` Right ()
            validateNoDuplicateQuotedIdents impArgs (ArgsArity []) `shouldBe` Right ()

    it "accepts one arg (impure args)" $ do
        property $ \(QuotedIdent ctx) -> do
            validateNoDuplicateQuotedIdents (both ctx) (ArgsArity []) `shouldBe` Right ()

    it "accepts one arg (varia)" $ do
        property $ \(QuotedIdent varargs) -> do
            forM_ [none, form] $ \impArgs -> do
                validateNoDuplicateQuotedIdents impArgs (ArgsVaria varargs) `shouldBe` Right ()

    it "accepts one arg (arity)" $ do
        property $ \(QuotedIdent arg) -> do
            forM_ [none, form] $ \impArgs -> do
                validateNoDuplicateQuotedIdents impArgs (ArgsArity [arg]) `shouldBe` Right ()

    it "accepts two args (impure+varia)" $ do
        property $ \(UniqueQuotedIdents2 ctx varargs) -> do
            validateNoDuplicateQuotedIdents (both ctx) (ArgsVaria varargs) `shouldBe` Right ()

    it "accepts two args (impure+arity)" $ do
        property $ \(UniqueQuotedIdents2 ctx arg) -> do
            validateNoDuplicateQuotedIdents (both ctx) (ArgsArity [arg]) `shouldBe` Right ()

    it "accepts two args (arity)" $ do
        property $ \(UniqueQuotedIdents2 arg1 arg2) -> do
            forM_ [none, form] $ \impArgs -> do
                validateNoDuplicateQuotedIdents impArgs (ArgsArity [arg1, arg2]) `shouldBe` Right ()

    it "accepts N+1 args (impure+arity)" $ do
        property $ \(UniqueQuotedIdentsNP1 ctx args) -> do
            validateNoDuplicateQuotedIdents (both ctx) (ArgsArity args) `shouldBe` Right ()

    it "rejects two args (impure+varia)" $ do
        property $ \(QuotedIdent x) -> do
            let (ctx, varargs) = (x, x)
            isLeft (validateNoDuplicateQuotedIdents (both ctx) (ArgsVaria varargs)) `shouldBe` True

    it "rejects two args (impure+arity)" $ do
        property $ \(QuotedIdent x) -> do
            let (ctx, arg) = (x, x)
            isLeft (validateNoDuplicateQuotedIdents (both ctx) (ArgsArity [arg])) `shouldBe` True

    it "rejects two args (arity)" $ do
        property $ \(QuotedIdent x) -> do
            let (arg1, arg2) = (x, x)
            forM_ [none, form] $ \impArgs -> do
                isLeft (validateNoDuplicateQuotedIdents none (ArgsArity [arg1, arg2])) `shouldBe` True

    it "rejects N+1 args (impure+arity)" $ do
        property $ \(UniqueQuotedIdentsNP1 ctx args) -> do
            isLeft (validateNoDuplicateQuotedIdents (both ctx) (ArgsArity $ ctx:args)) `shouldBe` True

unconsFuncBodySpec :: Spec
unconsFuncBodySpec = describe "unconsFuncBody" $ do
    it "rejects empty body" $ do
        isLeft (unconsFuncBody []) `shouldBe` True
    
    it "uncons one element" $ do
        property $ \v -> do
            unconsFuncBody [v] `shouldBe` Right (v, [])
    
    it "uncons two elements" $ do
        property $ \v1 v2 -> do
            unconsFuncBody [v1, v2] `shouldBe` Right (v1, [v2])
    
    it "uncons three elements" $ do
        property $ \v1 v2 v3 -> do
            unconsFuncBody [v1, v2, v3] `shouldBe` Right (v1, [v2, v3])
    
    it "uncons N+1 elements" $ do
        property $ \v vs -> do
            unconsFuncBody (v:vs) `shouldBe` Right (v, vs)

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

-- Utils
none = None
form = ArgPass $ Eval
both = Both (Eval)

data UniqueQuotedIdents2 = UniqueQuotedIdents2 Symb Symb deriving (Show, Eq)
instance Arbitrary UniqueQuotedIdents2 where
    arbitrary = do
        QuotedIdent x <- arbitrary
        QuotedIdent y <- arbitrary
        if [x, y] /= nub [x, y] then arbitrary else 
            return $ UniqueQuotedIdents2 x y

data UniqueQuotedIdentsNP1 = UniqueQuotedIdentsNP1 Symb [Symb] deriving (Show, Eq)
instance Arbitrary UniqueQuotedIdentsNP1 where
    arbitrary = do
        QuotedIdent x <- arbitrary
        xs <- arbFew $ do QuotedIdent x' <- arbitrary; return x'
        if x:xs /= nub (x:xs) then arbitrary else 
            return $ UniqueQuotedIdentsNP1 x xs
