module Ops.Func.FuncBodySpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Ops.SymbSpec
import TestUtils
import Types.Func.FuncBody

spec :: Spec
spec = return ()

-- Utils
instance ArbWithDepth a => Arbitrary (FuncBody a) where arbitrary = arbDepth
instance ArbWithDepth a => ArbWithDepth (FuncBody a) where
    arbWithDepth depth = oneof
        [ BodyBuiltIn <$> arbitrary
        , liftM2 BodyCustom (arbWithDepth depth) $ arbWithDepth depth
        ]