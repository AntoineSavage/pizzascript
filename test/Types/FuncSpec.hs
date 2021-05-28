module Types.FuncSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Ops.Func
import TestUtils
import Types.Func
import Types.Func.ArgPass
import Types.Func.FuncArgsSpec
import Types.Func.FuncBodySpec
import Types.Func.FuncImpureArgs
import Types.Func.FuncImpureArgsSpec

spec :: Spec
spec = describe "Func" $ do
    it "implements Show" $ do
        property $ \ia a b -> do
            show (Func ia a b :: Func Int) `shouldBe` "Func {impArgs = " ++ show ia ++ ", args = " ++ show a ++ ", body = " ++ show b ++ "}"

    it "implements Eq" $ do
        property $ \iax iay ax ay bx by -> do
            (Func iax ax bx :: Func Int) == Func iax ax bx `shouldBe` True
            (Func iax ax bx :: Func Int) == Func iay ax bx `shouldBe` iax == iay
            (Func iax ax bx :: Func Int) == Func iax ay bx `shouldBe` ax == ay
            (Func iax ax bx :: Func Int) == Func iax ax by `shouldBe` bx == by

    it "implements Ord" $ do
        property $ \iax iay ax ay bx by -> do
            (Func iax ax bx :: Func Int) <= Func iax ax bx `shouldBe` True
            (Func iax ax bx :: Func Int) <= Func iay ax bx `shouldBe` iax <= iay
            (Func iax ax bx :: Func Int) <= Func iax ay bx `shouldBe` ax <= ay
            (Func iax ax bx :: Func Int) <= Func iax ax by `shouldBe` bx <= by

-- Utils
instance ArbWithDepth a => Arbitrary (Func a) where arbitrary = arbDepth
instance ArbWithDepth a => ArbWithDepth (Func a) where
    arbWithDepth depth = liftM3 Func arbitrary arbitrary $ arbWithDepth depth