module Data.Func.FuncImpureArgsSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Func.ArgPassSpec
import Data.Func.FuncImpureArgs
import Data.SymbSpec

spec :: Spec
spec = return ()

-- Utils
instance Arbitrary FuncImpureArgs where
    arbitrary = oneof
        [ return None
        , ArgPass <$> arbitrary
        , liftM2 Both arbitrary arbitrary
        ]