module UtilsSpec where

import qualified Data.Map as M

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import TestUtils
import Types
import Utils

spec :: Spec
spec = do
    toFormSpec
    invalidArityMsgSpec
    f0Spec
    f1Spec
    f2Spec
    f3Spec

toFormSpec :: Spec
toFormSpec = describe "toForm" $ do
    it "converts empty list" $ do
        property $ \p -> do
            toForm p KindList [] `shouldBe` [AstExpr p $ AstIdent $ identList]
            toForm p KindDict [] `shouldBe` [AstExpr p $ AstIdent $ identDict]
            toForm p KindForm [] `shouldBe` []

    it "converts list" $ do
        property $ \p es -> do
            toForm p KindList es `shouldBe` (AstExpr p $ AstIdent $ identList) : es
            toForm p KindDict es `shouldBe` (AstExpr p $ AstIdent $ identDict) : es
            toForm p KindForm es `shouldBe` es

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
