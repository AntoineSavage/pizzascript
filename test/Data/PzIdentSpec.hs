module Data.PzIdentSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Either
import Data.PzIdent
import Text.Parsec

spec :: Spec
spec = describe "moo" $ it "moos!" $ 1+1 `shouldBe` 2