module Types.Func.FuncBodySpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import TestUtils
import Types.Func.FuncBody
import Types.Symb
import Types.SymbSpec

spec :: Spec
spec = describe "FuncBody" $ do
    it "implements Show" $ do
        property $ \s (Positive x) (Few xs) -> do
            show (BodyBuiltIn s :: FuncBody Int) `shouldBe` "BodyBuiltIn (" ++ show s ++ ")"
            show (BodyCustom x xs :: FuncBody Int) `shouldBe` "BodyCustom " ++ show x ++ " " ++ show xs
            show (BodyCustom (-x) xs :: FuncBody Int) `shouldBe` "BodyCustom (-" ++ show x ++ ") " ++ show xs

    it "implements Eq" $ do
        property $ \sx sy x y (Few xs) (Few ys) -> do
            (BodyBuiltIn sx :: FuncBody Int) == BodyBuiltIn sx `shouldBe` True
            (BodyBuiltIn sx :: FuncBody Int) == BodyBuiltIn sy `shouldBe` sx == sy
            (BodyBuiltIn undefined :: FuncBody Int) == BodyCustom undefined undefined `shouldBe` False

            (BodyCustom undefined undefined :: FuncBody Int) == BodyBuiltIn undefined `shouldBe` False
            (BodyCustom x xs :: FuncBody Int) == BodyCustom x xs `shouldBe` True
            (BodyCustom x xs :: FuncBody Int) == BodyCustom y xs `shouldBe` x == y
            (BodyCustom x xs :: FuncBody Int) == BodyCustom x ys `shouldBe` xs == ys

    it "implements Ord" $ do
        property $ \sx sy x y (Few xs) (Few ys) -> sx /= sy && x /= y && xs /= ys ==> do
            (BodyBuiltIn sx :: FuncBody Int) <= BodyBuiltIn sx `shouldBe` True
            (BodyBuiltIn sx :: FuncBody Int) <= BodyBuiltIn sy `shouldBe` sx <= sy
            (BodyBuiltIn undefined :: FuncBody Int) <= BodyCustom undefined undefined `shouldBe` True

            (BodyCustom undefined undefined :: FuncBody Int) <= BodyBuiltIn undefined `shouldBe` False
            (BodyCustom x xs :: FuncBody Int) <= BodyCustom x xs `shouldBe` True
            (BodyCustom x xs :: FuncBody Int) <= BodyCustom y xs `shouldBe` x <= y
            (BodyCustom x xs :: FuncBody Int) <= BodyCustom x ys `shouldBe` xs <= ys

-- Utils
instance ArbWithDepth a => Arbitrary (FuncBody a) where arbitrary = arbDepth
instance ArbWithDepth a => ArbWithDepth (FuncBody a) where
    arbWithDepth depth = oneof
        [ BodyBuiltIn <$> arbitrary
        , liftM2 BodyCustom (arbWithDepth depth) $ arbWithDepth depth
        ]