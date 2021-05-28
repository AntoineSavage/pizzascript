module Types.Func.FuncArgsSpec where

import Test.Hspec
import Test.QuickCheck

import Ops.SymbSpec
import TestUtils
import Types.Func.FuncArgs

spec :: Spec
spec = describe "FuncArgs" $ do
    it "implements Show" $ do
        property $ \x (Few xs) -> do
            show (ArgsVaria x) `shouldBe` "ArgsVaria (" ++ show x ++ ")"
            show (ArgsArity xs) `shouldBe` "ArgsArity " ++ show xs

    it "implements Eq" $ do
        property $ \x y (Few xs) (Few ys) -> do
            ArgsVaria x == ArgsVaria x `shouldBe` True
            ArgsVaria x == ArgsVaria y `shouldBe` x == y
            ArgsVaria undefined == ArgsArity undefined `shouldBe` False

            ArgsArity undefined == ArgsVaria undefined `shouldBe` False
            ArgsArity xs == ArgsArity xs `shouldBe` True
            ArgsArity xs == ArgsArity ys `shouldBe` xs == ys

    it "implements Ord" $ do
        property $ \x y (Few xs) (Few ys) -> do
            ArgsVaria x <= ArgsVaria x `shouldBe` True
            ArgsVaria x <= ArgsVaria y `shouldBe` x <= y
            ArgsVaria undefined <= ArgsArity undefined `shouldBe` True

            ArgsArity undefined <= ArgsVaria undefined `shouldBe` False
            ArgsArity xs <= ArgsArity xs `shouldBe` True
            ArgsArity xs <= ArgsArity ys `shouldBe` xs <= ys

-- Utils
instance Arbitrary FuncArgs where
    arbitrary = oneof
        [ ArgsVaria <$> arbQuotedIdent
        , ArgsArity <$> arbFew arbQuotedIdent
        ]