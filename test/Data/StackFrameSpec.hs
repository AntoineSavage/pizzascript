module Data.StackFrameSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.FuncSpec
import Data.PzValSpec
import Data.StackFrame
import TestUtils

spec :: Spec
spec = return ()

-- Utils
instance Arbitrary StackFrame where arbitrary = arbDepth
instance ArbWithDepth StackFrame where
    arbWithDepth depth = oneof
        [liftM2 Block arbDepth arbDepth
        , liftM4 Form arbDepth arbitrary arbitrary arbDepth
        , do
            a <- arbDepth
            b <- arbitrary
            c <- arbDepth
            d <- arbDepth
            e <- arbDepth
            f <- arbDepth
            g <- arbDepth
            return $ Invoc a b c d e f g
        ]