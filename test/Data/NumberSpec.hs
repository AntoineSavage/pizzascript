module Data.NumberSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Number as Number
import Data.Either
import Text.Parsec
import Utils

spec :: Spec
spec = do
  parserSpec

parserSpec = do
  describe "parser" $ do
    it "parses positive ints" $ do
       parse parser "test" "0" `shouldBe` Right (NumberInt 0)