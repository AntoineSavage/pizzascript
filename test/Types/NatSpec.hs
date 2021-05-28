module Types.NatSpec where

import Test.Hspec
import Test.QuickCheck

import TestUtils
import Types.Nat

spec :: Spec
spec = describe "Nat" $ do
    it "implements Show" $ do
        property $ \x -> do
            show Z `shouldBe` "Z"
            show (S Z) `shouldBe` "S Z"
            show (S (S Z)) `shouldBe` "S (S Z)"

            let x' = S x
            show (S x') `shouldBe` "S (" ++ show x' ++ ")"

    it "implements Eq" $ do
        property $ \x -> do
            x == x `shouldBe` True
            x == S x `shouldBe` False
            S x == x `shouldBe` False
            S x == S x `shouldBe` True

    it "implements Ord" $ do
        property $ \x -> do
            Z <= x `shouldBe` True

            let x' = S x
            x' <= Z `shouldBe` False

            x <= S x `shouldBe` True
            S x <= x `shouldBe` False

-- Utils
instance Arbitrary Nat where
    arbitrary = oneof
        [ return Z
        , return $ S Z
        , do
            Positive n <- arbitrary
            let go k = if k <= 0 then Z else S $ go $ k-1
            return $ go (n :: Int)
        ]