module Types.Func.ArgPassSpec where

import Test.Hspec
import Test.QuickCheck

import TestUtils
import Types.Func.ArgPass

spec :: Spec
spec = describe "ArgPass" $ do
    it "implements Show" $ do
        show Eval `shouldBe` "Eval"
        show Quote `shouldBe` "Quote"
        show Unquote `shouldBe` "Unquote"
        show DeepQuote `shouldBe` "DeepQuote"
        show DeepUnquote `shouldBe` "DeepUnquote"

    it "implements Eq" $ do
        Eval == Eval `shouldBe` True
        Eval == Quote `shouldBe` False
        Eval == Unquote `shouldBe` False
        Eval == DeepQuote `shouldBe` False
        Eval == DeepUnquote `shouldBe` False

        Quote == Eval `shouldBe` False
        Quote == Quote `shouldBe` True
        Quote == Unquote `shouldBe` False
        Quote == DeepQuote `shouldBe` False
        Quote == DeepUnquote `shouldBe` False

        Unquote == Eval `shouldBe` False
        Unquote == Quote `shouldBe` False
        Unquote == Unquote `shouldBe` True
        Unquote == DeepQuote `shouldBe` False
        Unquote == DeepUnquote `shouldBe` False

        DeepQuote == Eval `shouldBe` False
        DeepQuote == Quote `shouldBe` False
        DeepQuote == Unquote `shouldBe` False
        DeepQuote == DeepQuote `shouldBe` True
        DeepQuote == DeepUnquote `shouldBe` False

        DeepUnquote == Eval `shouldBe` False
        DeepUnquote == Quote `shouldBe` False
        DeepUnquote == Unquote `shouldBe` False
        DeepUnquote == DeepQuote `shouldBe` False
        DeepUnquote == DeepUnquote `shouldBe` True

    it "implements Ord" $ do
        Eval <= Eval `shouldBe` True
        Eval <= Quote `shouldBe` True
        Eval <= Unquote `shouldBe` True
        Eval <= DeepQuote `shouldBe` True
        Eval <= DeepUnquote `shouldBe` True

        Quote <= Eval `shouldBe` False
        Quote <= Quote `shouldBe` True
        Quote <= Unquote `shouldBe` True
        Quote <= DeepQuote `shouldBe` True
        Quote <= DeepUnquote `shouldBe` True

        Unquote <= Eval `shouldBe` False
        Unquote <= Quote `shouldBe` False
        Unquote <= Unquote `shouldBe` True
        Unquote <= DeepQuote `shouldBe` True
        Unquote <= DeepUnquote `shouldBe` True

        DeepQuote <= Eval `shouldBe` False
        DeepQuote <= Quote `shouldBe` False
        DeepQuote <= Unquote `shouldBe` False
        DeepQuote <= DeepQuote `shouldBe` True
        DeepQuote <= DeepUnquote `shouldBe` True

        DeepUnquote <= Eval `shouldBe` False
        DeepUnquote <= Quote `shouldBe` False
        DeepUnquote <= Unquote `shouldBe` False
        DeepUnquote <= DeepQuote `shouldBe` False
        DeepUnquote <= DeepUnquote `shouldBe` True

-- Utils
argPasses = [ Eval, Quote, Unquote, DeepQuote, DeepUnquote ]

instance Arbitrary ArgPass where arbitrary = elements argPasses
instance ArbWithDepth ArgPass where arbWithDepth _ = arbitrary