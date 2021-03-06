module Ops.NatSpec where

import Test.Hspec
import Test.QuickCheck

import Ops.Nat
import Types.Nat
import Types.NatSpec

spec :: Spec
spec = do
    lenVsUnlenSpec
    lenSpec
    unlenSpec

lenVsUnlenSpec :: Spec
lenVsUnlenSpec = describe "len vs unlen" $ do
    it "composes len and unlen into id" $ do
        property $ \n -> do
            len (unlen n undefined) `shouldBe` n

lenSpec :: Spec
lenSpec = describe "len" $ do
    it "lens zero elements" $ do
        len [] `shouldBe` Z

    it "lens one element" $ do
        len [()] `shouldBe` (S Z)

    it "lens two elements" $ do
        len [(),()] `shouldBe` (S $ S Z)

    it "lens three elements" $ do
        len [(),(),()] `shouldBe` (S $ S $ S $ Z)

    it "lens n elements" $ do
        property $ \n ->
            len (replicate (toInt n) ()) `shouldBe` n

unlenSpec :: Spec
unlenSpec = describe "unlen" $ do
    it "unlens zero elements" $ do
        unlen Z () `shouldBe` []

    it "unlens one element" $ do
        unlen (S Z) () `shouldBe` [()]

    it "unlens two elements" $ do
        unlen (S $ S Z) () `shouldBe` [(),()]

    it "unlens three elements" $ do
        unlen (S $ S $ S Z) () `shouldBe` [(),(),()]

    it "unlens n elements list" $ do
        property $ \n -> do
            unlen n () `shouldBe` replicate (toInt n) ()

-- Utils
toInt :: Nat -> Int
toInt k = case k of
    Z -> 0
    S n -> 1 + toInt n