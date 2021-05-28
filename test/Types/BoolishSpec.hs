module Types.BoolishSpec where

import Test.Hspec
import Test.QuickCheck

import Types.Boolish

spec :: Spec
spec = describe "Boolish" $ do
    it "implements Show" $ do
        show FalseReal `shouldBe` "FalseReal"
        show Falsish `shouldBe` "Falsish"
        show Truish `shouldBe` "Truish"
        show TrueReal `shouldBe` "TrueReal"

    it "implements Eq" $ do
        FalseReal == FalseReal `shouldBe` True
        FalseReal == Falsish `shouldBe` False
        FalseReal == Truish `shouldBe` False
        FalseReal == TrueReal `shouldBe` False

        Falsish == FalseReal `shouldBe` False
        Falsish == Falsish `shouldBe` True
        Falsish == Truish `shouldBe` False
        Falsish == TrueReal `shouldBe` False

        Truish == FalseReal `shouldBe` False
        Truish == Falsish `shouldBe` False
        Truish == Truish `shouldBe` True
        Truish == TrueReal `shouldBe` False

        TrueReal == FalseReal `shouldBe` False
        TrueReal == Falsish `shouldBe` False
        TrueReal == Truish `shouldBe` False
        TrueReal == TrueReal `shouldBe` True