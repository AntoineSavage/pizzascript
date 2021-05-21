module BuiltIns.PosSpec where

import Test.Hspec
import Test.QuickCheck

import BuiltIns.Pos
import Data.WithPos
import Text.Parsec.Pos

spec :: Spec
spec = do
    builtInPosSpec
    withPosSpec

builtInPosSpec :: Spec
builtInPosSpec = describe "builtInPos" $ do
    it "contains the expected fields" $ do
        sourceName builtInPos `shouldBe` "<built-in>"
        sourceLine builtInPos `shouldBe` 0
        sourceColumn builtInPos `shouldBe` 0

withPosSpec :: Spec
withPosSpec = describe "withPos" $ do
    it "adds built-in pos" $ do
        pos (withPos ()) `shouldBe` builtInPos