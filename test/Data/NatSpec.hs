module Data.NatSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Nat

spec :: Spec
spec = do
    lenVsUnlenSpec
    lenSpec
    unlenSpec

lenVsUnlenSpec :: Spec
lenVsUnlenSpec = describe "len vs unlen" $ do
    it "composes len and unlen into id" $ do
        property $ \n m -> do
            n + m `shouldBe` m + (n :: Int)
    -- TODO

lenSpec :: Spec
lenSpec = describe "len" $ do
    it "consumes list" $ do
        property $ \n -> do
            n `shouldBe` (n :: Int)
    -- TODO

unlenSpec :: Spec
unlenSpec = describe "unlen" $ do
    it "produces list" $ do
        property $ \n -> do
            n `shouldBe` (n :: Int)
    -- TODO

instance Arbitrary Nat where
    arbitrary = return Z -- TODO