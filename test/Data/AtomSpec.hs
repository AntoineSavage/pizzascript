module Data.AtomSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Atom (parser)
import Data.Either
import Text.Parsec

spec :: Spec
spec = do
  describe "parser" $ do
    it "fails for empty string" $ do
      isLeft(parse parser "tests" "") `shouldBe` True