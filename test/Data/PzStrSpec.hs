module Data.PzStrSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Either
import Data.PzStr
import Text.Parsec

spec :: Spec
spec = do
    parseSpec
    unparseSpec

parseSpec :: Spec
parseSpec = describe "parse" $ do
    it "doesn't do anything yet" $ do
        "\x10FFFF" `shouldBe` ""

unparseSpec :: Spec
unparseSpec = describe "unparse" $ do
    it "doesn't do anything yet" $ do
        1+1 `shouldBe` 2