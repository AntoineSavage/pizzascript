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
    boolishSpec
    invalidArityMsgSpec
    f0Spec
    f1Spec
    f2Spec
    f3Spec

toFormSpec :: Spec
toFormSpec = describe "toForm" $ do
    it "converts empty list" $ do
        toForm pos KindList [] `shouldBe` [AstExpr pos "" $ AstIdent $ identList]
        toForm pos KindDict [] `shouldBe` [AstExpr pos "" $ AstIdent $ identDict]
        toForm pos KindForm [] `shouldBe` []

    it "converts list" $ do
        property $ \es -> do
            toForm pos KindList es `shouldBe` (AstExpr pos "" $ AstIdent $ identList) : es
            toForm pos KindDict es `shouldBe` (AstExpr pos "" $ AstIdent $ identDict) : es
            toForm pos KindForm es `shouldBe` es

boolishSpec :: Spec
boolishSpec = describe "boolish" $ do
    it "returns false for falsish values" $ do
        boolish PzUnit `shouldBe` False
        boolish (PzNum 0) `shouldBe` False
        boolish (PzStr "") `shouldBe` False
        boolish (PzSymb $ symb $ ident "false") `shouldBe` False
        boolish (PzList []) `shouldBe` False
        boolish (PzDict M.empty) `shouldBe` False
    
    it "returns true for truish values" $ do
        boolish (PzNum 1) `shouldBe` True
        boolish (PzStr "abc") `shouldBe` True
        boolish (PzSymb $ symb $ ident "abc") `shouldBe` True
        boolish (PzList [PzUnit]) `shouldBe` True
        boolish (PzDict $ M.fromList [(PzUnit, PzUnit)]) `shouldBe` True
        boolish (PzFunc $ Func Eval Nothing (ArgsArity []) (BodyBuiltIn "func")) `shouldBe` True

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
            f0 [] (\x -> if [x] /= [()] then undefined else r) `shouldBe` r

    it "fails with one or more arguments" $ do
        property $ \v vs -> do
            let xs = v:vs
            f0 xs undefined `shouldBe` Left (invalidArityMsg 0 xs)

f1Spec :: Spec
f1Spec = describe "f1" $ do
    it "succeeds with one argument" $ do
        property $ \r v -> do
            f1 [v] (\x -> if [x] /= [v] then undefined else r) `shouldBe` r

    it "fails with zero, two or more arguments" $ do
        property $ \v1 v2 vs -> do
            let xs = v1:v2:vs
            f1 [] undefined `shouldBe` Left (invalidArityMsg 1 [])
            f1 xs undefined `shouldBe` Left (invalidArityMsg 1 xs)

f2Spec :: Spec
f2Spec = describe "f2" $ do
    it "succeeds with two arguments" $ do
        property $ \r v1 v2 -> do
            f2 [v1,v2] (\x y -> if [x,y] /= [v1,v2] then undefined else r) `shouldBe` r

    it "fails with zero, one, three or more arguments" $ do
        property $ \v1 v2 v3 vs -> do
            let xs = v1:v2:v3:vs
            f2 [] undefined `shouldBe` Left (invalidArityMsg 2 [])
            f2 [v1] undefined `shouldBe` Left (invalidArityMsg 2 [v1])
            f2 xs undefined `shouldBe` Left (invalidArityMsg 2 xs)

f3Spec :: Spec
f3Spec = describe "f3" $ do
    it "succeeds with three arguments" $ do
        property $ \r v1 v2 v3 -> do
            f3 [v1,v2,v3] (\x y z -> if [x,y,z] /= [v1,v2,v3] then undefined else r) `shouldBe` r

    it "fails with zero, one, two, four or more arguments" $ do
        property $ \v1 v2 v3 v4 vs -> do
            let xs = v1:v2:v3:v4:vs
            f3 [] undefined `shouldBe` Left (invalidArityMsg 3 [])
            f3 [v1] undefined `shouldBe` Left (invalidArityMsg 3 [v1])
            f3 [v1,v2] undefined `shouldBe` Left (invalidArityMsg 3 [v1,v2])
            f3 xs undefined `shouldBe` Left (invalidArityMsg 3 xs)