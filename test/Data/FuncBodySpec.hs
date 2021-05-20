module Data.FuncBodySpec where

import Test.Hspec
import Test.QuickCheck

import Data.AstExprSpec
import Data.FuncBody
import Data.WithPosSpec
import TestUtils
import Utils.ArbWithDepth

spec :: Spec
spec = return ()

-- Utils
instance Arbitrary FuncBody where arbitrary = arbDepth
instance ArbWithDepth FuncBody where arbWithDepth depth = oneof [BodyBuiltIn <$> arbitrary, BodyCustom <$> arbFew (arbWithDepth depth)]