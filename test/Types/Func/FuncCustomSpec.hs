module Types.Func.FuncCustomSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.List
import Ops.PzValSpec
import TestUtils
import Types.Func.FuncArgs
import Types.Func.FuncCustom
import Types.Func.FuncImpureArgs
import Types.PzVal

spec :: Spec
spec = describe "FuncCustom" $ do
    it "implements Show" $ do
        property $ \ap ec a x (Few xs) -> x /= PzUnit ==> do
            show (FuncCustom None a PzUnit xs) `shouldBe` "FuncCustom None" ++ " (" ++ show a ++ ") PzUnit " ++ show xs
            show (FuncCustom (ArgPass ap) a x xs) `shouldBe` "FuncCustom (" ++ show (ArgPass ap) ++ ") (" ++ show a ++ ") (" ++ show x ++ ") " ++ show xs
            show (FuncCustom (Both ap ec) a x xs) `shouldBe` "FuncCustom (" ++ show (Both ap ec) ++ ") (" ++ show a ++ ") (" ++ show x ++ ") " ++ show xs

    it "implements Eq" $ do
        property $ \iax iay ax ay x y (Few xs) (Few ys) -> do
            FuncCustom iax ax x xs == FuncCustom iax ax x xs `shouldBe` True
            FuncCustom iax ax x xs == FuncCustom iay ax x xs `shouldBe` iax == iay
            FuncCustom iax ax x xs == FuncCustom iax ay x xs `shouldBe` ax == ay
            FuncCustom iax ax x xs == FuncCustom iax ax y xs `shouldBe` x == y
            FuncCustom iax ax x xs == FuncCustom iax ax x ys `shouldBe` xs == ys

-- Utils
instance Arbitrary FuncCustom where arbitrary = arbDepth
instance ArbWithDepth FuncCustom where
    arbWithDepth depth = do
        let getImpArgsIdents (Both _ s) = [s]
            getImpArgsIdents _          = []

            getArgsIdents (ArgsVaria s)  = [s]
            getArgsIdents (ArgsArity ss) = ss

        impArgs <- arbitrary
        args <- arbitrary
        let ss = getImpArgsIdents impArgs ++ getArgsIdents args
        if ss == nub ss
            then liftM2 (FuncCustom impArgs args) (arbWithDepth depth) $ arbWithDepth depth
            else arbWithDepth depth