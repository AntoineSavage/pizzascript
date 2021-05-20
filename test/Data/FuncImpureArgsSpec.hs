module Data.FuncImpureArgsSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.ArgPassSpec
import Data.FuncImpureArgs
import Data.IdentSpec
import Data.WithPosSpec

spec :: Spec
spec = return ()

-- Utils
instance Arbitrary FuncImpureArgs where
    arbitrary = oneof
        [ return None
        , liftM2 ArgPass arbitrary arbitrary
        , liftM3 Both arbitrary arbitrary arbitrary
        ]