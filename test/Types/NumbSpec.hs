module Types.NumbSpec where

import Test.Hspec
import Test.QuickCheck

import Types.Numb

spec :: Spec
spec = describe "Nat" $ do
    it "implements Show" $ do
        property $ \(Positive x) -> do
            show (Numb x) `shouldBe` "Numb " ++ show x
            show (Numb $ -x) `shouldBe` "Numb (" ++ show (-x) ++ ")"

    it "implements Eq" $ do
        property $ \x y -> do
            Numb x == Numb x `shouldBe` True
            Numb x == Numb y `shouldBe` x == y

    it "implements Ord" $ do
        property $ \x y -> do
            Numb x <= Numb x `shouldBe` True
            Numb x <= Numb y `shouldBe` x <= y

-- Utils
instance Arbitrary Numb where arbitrary = Numb <$> arbitrary