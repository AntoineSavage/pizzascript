module Data.FuncArgsSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.FuncArgs
import Data.IdentSpec
import Data.WithPosSpec
import TestUtils

spec :: Spec
spec = return ()

-- Utils
instance Arbitrary FuncArgs where
    arbitrary = oneof [ArgsVaria <$> arbitrary, liftM2 ArgsArity arbitrary $ arbFew arbitrary]