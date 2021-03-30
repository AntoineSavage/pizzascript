module Data.NumIntSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.NumInt as NumInt
import Data.Either
import Text.Parsec
import Utils

spec :: Spec
spec = do
  describe "parser" $ do
    it "is trivial" $ do
      True `shouldBe` True