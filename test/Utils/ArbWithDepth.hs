module Utils.ArbWithDepth where

import Test.QuickCheck

class Arbitrary a => ArbWithDepth a where
    arbWithDepth :: Int -> Gen a