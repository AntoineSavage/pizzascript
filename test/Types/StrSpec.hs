module Types.StrSpec where

import Test.Hspec
import Test.QuickCheck

import Types.Str

spec :: Spec
spec = describe "Str" $ do
    it "implements Show" $ do
        property $ \x -> do
            show (Str x) `shouldBe` "Str " ++ show x

    it "implements Eq" $ do
        property $ \x y -> do
            Str x == Str x `shouldBe` True
            Str x == Str y `shouldBe` x == y

    it "implements Ord" $ do
        property $ \x y -> do
            Str x <= Str x `shouldBe` True
            Str x <= Str y `shouldBe` x <= y

-- Utils
instance Arbitrary Str where arbitrary = Str <$> arbitrary