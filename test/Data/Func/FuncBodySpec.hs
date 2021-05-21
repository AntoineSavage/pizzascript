module Data.Func.FuncBodySpec where

import Test.Hspec
import Test.QuickCheck

import Data.AstExprSpec
import Data.Func.FuncBody
import Data.WithPosSpec
import TestUtils

spec :: Spec
spec = return ()

-- Utils
instance Arbitrary FuncBody where arbitrary = arbDepth
instance ArbWithDepth FuncBody where arbWithDepth depth = oneof [BodyBuiltIn <$> arbitrary, BodyCustom <$> arbFew (arbWithDepth depth)]