module UtilsSpec where

import qualified Data.Map as M

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.ArgPass
import Data.AstExpr
import Data.Either
import Data.FuncArgs
import Data.FuncImpureArgs
import Data.FuncBody
import Data.Ident
import Data.List
import Data.Lst
import Data.Nat
import Data.Numb
import Data.NumbSpec
import Data.Str
import Data.StrSpec
import Data.Symb
import Data.SymbSpec
import Data.WithPos
import Idents
import Symbs
import TestUtils
import TestUtils2
import Types
import Utils

spec :: Spec
spec = do
    toFormSpec
    getIdentSpec
    getArgPassSpec
    getDuplicatesSpec
    addIdentAndPosSpec
    invalidArityMsgSpec
    f0Spec
    f1Spec
    f2Spec
    f3Spec
    fpureSpec
    setCtxSpec
    toFuncCustomVsFromFuncCustomSpec
    toFuncCustomSpec
    fromFuncCustomSpec

toFormSpec :: Spec
toFormSpec = describe "toForm" $ do
    it "converts empty list" $ do
        property $ \p -> do
            toForm p KindList [] `shouldBe` [WithPos p $ AstIdent $ identList]
            toForm p KindDict [] `shouldBe` [WithPos p $ AstIdent $ identDict]
            toForm p KindForm [] `shouldBe` []

    it "converts list" $ do
        property $ \p es -> do
            toForm p KindList es `shouldBe` (WithPos p $ AstIdent $ identList) : es
            toForm p KindDict es `shouldBe` (WithPos p $ AstIdent $ identDict) : es
            toForm p KindForm es `shouldBe` es

getIdentSpec :: Spec
getIdentSpec = describe "getIdent" $ do
    it "converts ident" $
        property $ \p ident ->
            getIdent (WithPos p $ AstIdent ident) `shouldBe` Right (WithPos p ident)

    it "rejects number" $ do
        property $ \p n ->
            isLeft (getIdent (WithPos p $ AstNum n)) `shouldBe` True

    it "rejects string" $ do
        property $ \p s ->
            isLeft (getIdent (WithPos p $ AstStr s)) `shouldBe` True

    it "rejects symbol" $ do
        property $ \p s ->
            isLeft (getIdent (WithPos p $ AstSymb s)) `shouldBe` True

    it "rejects list" $ do
        property $ \p k l ->
            isLeft (getIdent (WithPos p $ AstList $ Lst k l)) `shouldBe` True

getArgPassSpec :: Spec
getArgPassSpec = describe "getArgPass" $ do
    it "converts None to Eval" $ do
        property $ \implCtx args body ->
            getArgPass (Func implCtx None args body) `shouldBe` Eval

    it "converts ArgPass" $ do
        property $ \p implCtx ap args body ->
            getArgPass (Func implCtx (ArgPass p $ WithPos p ap) args body) `shouldBe` ap

    it "converts Both" $ do
        property $ \p implCtx ap explCtx args body ->
            getArgPass (Func implCtx (Both p (WithPos p ap) explCtx) args body) `shouldBe` ap

getDuplicatesSpec :: Spec
getDuplicatesSpec = describe "getDuplicates" $ do
    it "finds no duplicates in empty list" $ do
        getDuplicates [] `shouldBe` ([] :: [Int])

    it "finds no duplicates" $ do
        property $ \(Uniques xs) -> do
            getDuplicates xs `shouldBe` ([] :: [Int])

    it "finds one duplicate" $ do
        property $ \x (Uniques xs) -> do
            getDuplicates (x:xs ++ [x]) `shouldBe` ([x] :: [Int])
   
    it "finds two duplicates" $ do
        property $ \x y (Uniques xs) -> nub [x, y] == [x, y] ==> do
            getDuplicates (x:y:xs ++ [x, y]) `shouldBe` (sort [x, y] :: [Int])
   
    it "finds three duplicates" $ do
        property $ \x y z (Uniques xs) -> nub [x, y, z] == [x, y, z] ==> do
            getDuplicates (x:y:z:xs ++ [x, y, z]) `shouldBe` (sort [x, y, z] :: [Int])
   
    it "finds N duplicates" $ do
        property $ \(Uniques ds) (Uniques xs) -> do
            getDuplicates (ds ++ xs ++ ds) `shouldBe` (sort ds :: [Int])
   
    it "finds only duplicates" $ do
        property $ \(Uniques xs) -> do
            getDuplicates (xs ++ xs) `shouldBe` (sort xs :: [Int])
   
addIdentAndPosSpec :: Spec
addIdentAndPosSpec = describe "addIdentAndPos" $ do
    it "adds position without identifier" $ do
        property $ \p s ->
            addIdentAndPos p Nothing s `shouldBe` s ++ "\n at:" ++ show p

    it "adds position with identifier" $ do
        property $ \p i s ->
            addIdentAndPos p (Just i) s `shouldBe` s ++ "\n at " ++ show i ++ ": " ++ show p

invalidArityMsgSpec :: Spec
invalidArityMsgSpec = describe "invalidArityMsg" $ do
    it "returns the appropriate message" $ do
        property $ \n (Positive m) -> do
            let xs = replicate m ()
            invalidArityMsg n xs `shouldBe` "Invalid number of arguments. Expected " ++ show n ++ ", got: " ++ show m

f0Spec :: Spec
f0Spec = describe "f0" $ do
    it "succeeds with zero arguments" $ do
        property $ \r -> do
            f0 [] (undefinedOrResult0 r) `shouldBe` r

    it "fails with one or more arguments" $ do
        property $ \v vs -> do
            let xs = v:vs :: [Int]
            f0 xs undef0 `shouldBe` Left (invalidArityMsg 0 xs)

f1Spec :: Spec
f1Spec = describe "f1" $ do
    it "succeeds with one argument" $ do
        property $ \r v -> do
            f1 [v] (undefinedOrResult1 v r) `shouldBe` r

    it "fails with zero, two or more arguments" $ do
        property $ \v1 v2 vs -> do
            let xs = v1:v2:vs
            f1 [] undef1 `shouldBe` Left (invalidArityMsg 1 [])
            f1 xs undef1 `shouldBe` Left (invalidArityMsg 1 xs)

f2Spec :: Spec
f2Spec = describe "f2" $ do
    it "succeeds with two arguments" $ do
        property $ \r v1 v2 -> do
            f2 [v1,v2] (undefinedOrResult2 v1 v2 r) `shouldBe` r

    it "fails with zero, one, three or more arguments" $ do
        property $ \v1 v2 v3 vs -> do
            let xs = v1:v2:v3:vs
            f2 [] undef2 `shouldBe` Left (invalidArityMsg 2 [])
            f2 [v1] undef2 `shouldBe` Left (invalidArityMsg 2 [v1])
            f2 xs undef2 `shouldBe` Left (invalidArityMsg 2 xs)

f3Spec :: Spec
f3Spec = describe "f3" $ do
    it "succeeds with three arguments" $ do
        property $ \r v1 v2 v3 -> do
            f3 [v1,v2,v3] (undefinedOrResult3 v1 v2 v3 r) `shouldBe` r

    it "fails with zero, one, two, four or more arguments" $ do
        property $ \v1 v2 v3 v4 vs -> do
            let xs = v1:v2:v3:v4:vs
            f3 [] undef3 `shouldBe` Left (invalidArityMsg 3 [])
            f3 [v1] undef3 `shouldBe` Left (invalidArityMsg 3 [v1])
            f3 [v1,v2] undef3 `shouldBe` Left (invalidArityMsg 3 [v1,v2])
            f3 xs undef3 `shouldBe` Left (invalidArityMsg 3 xs)

fpureSpec :: Spec
fpureSpec = describe "fpure" $ do
    it "wraps in a Right tuple" $ do
        property $ \(ArbDict ctx) r -> do
            fpure ctx r `shouldBe` Right (ctx, (r :: Int))

setCtxSpec :: Spec
setCtxSpec = describe "setCtx" $ do
    it "returns empty list unchanged" $ do
        setCtx undefined [] `shouldBe` []

    it "sets context on block frame" $ do
        property $ \(ArbDict ctx) (Few es) (Few fs) -> do
            setCtx ctx (Block undefined es:fs) `shouldBe` (Block ctx es:fs)

    it "sets context on form frame" $ do
        property $ \(ArbDict ctx) p mfi (Few es) (Few fs) -> do
            setCtx ctx (Form undefined p mfi es:fs) `shouldBe` (Form ctx p mfi es:fs)

    it "sets context on invoc frame (with args left to eval)" $ do
        property $ \(ArbDict ctx) p mfi f (Few as) (Few es) (Few fs) -> do
            setCtx ctx (Invoc undefined p mfi f as (Just es):fs) `shouldBe` (Invoc ctx p mfi f as (Just es):fs)

    it "sets context on invoc frame (with no args left to eval)" $ do
        property $ \(ArbDict ctx) p mfi f (Few as) (Few fs) -> do
            setCtx ctx (Invoc undefined p mfi f as Nothing:fs) `shouldBe` (Invoc ctx p mfi f as Nothing:fs)

toFuncCustomVsFromFuncCustomSpec :: Spec
toFuncCustomVsFromFuncCustomSpec = describe "toFuncCustom vs fromFuncCustom" $ do
    it "composes toFuncCustom and fromFuncCustom into id" $ do
        property $ \(ArbDict ctx) funcCustom -> do
            let func = fromFuncCustom ctx funcCustom
            toFuncCustom func `shouldBe` Right funcCustom
            fromFuncCustom ctx <$> toFuncCustom func `shouldBe` Right func

toFuncCustomSpec :: Spec
toFuncCustomSpec = describe "toFuncCustom" $ do
    it "rejects built-in function" $ do
        property $ \impArgs args ident ->
            toFuncCustom (Func undefined impArgs args $ BodyBuiltIn ident) `shouldBe` Left ident

    it "converts custom function" $ do
        property $ \impArgs args es ->
            toFuncCustom (Func undefined impArgs args $ BodyCustom es) `shouldBe` Right (FuncCustom impArgs args es)

fromFuncCustomSpec :: Spec
fromFuncCustomSpec = describe "fromFuncCustom" $ do
    it "converts to function (smallest)" $ do
        property $ \p -> do
            fromFuncCustom M.empty (FuncCustom None (ArgsArity p []) []) `shouldBe` Func M.empty None (ArgsArity p []) (BodyCustom [])

    it "converts to function (prop)" $ do
        property $ \(ArbDict implCtx) impArgs args (Few es) -> do
            fromFuncCustom implCtx (FuncCustom impArgs args es) `shouldBe` Func implCtx impArgs args (BodyCustom es)

-- Utils
undefinedOrResult0 :: Either String Int -> () -> Either String Int
undefinedOrResult0 r _ = r

undefinedOrResult1 :: Int -> Either String Int -> Int -> Either String Int
undefinedOrResult1 v r x = if [x] /= [v] then undefined else r

undefinedOrResult2 :: Int -> Int -> Either String Int -> Int -> Int -> Either String Int
undefinedOrResult2 v1 v2 r x1 x2 = if [x1, x2] /= [v1, v2] then undefined else r

undefinedOrResult3 :: Int -> Int -> Int -> Either String Int -> Int -> Int -> Int -> Either String Int
undefinedOrResult3 v1 v2 v3 r x1 x2 x3 = if [x1, x2, x3] /= [v1, v2, v3] then undefined else r

undef0 :: () -> Either String Int
undef0 = undefined

undef1 :: Int -> Either String Int
undef1 = undefined

undef2 :: Int -> Int -> Either String Int
undef2 = undefined

undef3 :: Int -> Int -> Int -> Either String Int
undef3 = undefined
