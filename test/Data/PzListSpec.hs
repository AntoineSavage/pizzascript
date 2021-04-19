module Data.PzListSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Either
import Data.PzList
import Text.Parsec

spec :: Spec
spec = do
    parseVsUnparseSpec
    parseSpec
    unparseSpec

parseVsUnparseSpec :: Spec
parseVsUnparseSpec = describe "parse vs unparse" $ do
    it "moos" $ 1+1 `shouldBe` 2

parseSpec :: Spec
parseSpec = describe "parse" $ do
    it "moos" $ 1+1 `shouldBe` 2

unparseSpec :: Spec
unparseSpec = describe "unparse" $ do
    it "moos" $ 1+1 `shouldBe` 2