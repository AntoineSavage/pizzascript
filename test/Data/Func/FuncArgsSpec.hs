module Data.Func.FuncArgsSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Func.FuncArgs
import Data.SymbSpec
import TestUtils

spec :: Spec
spec = return ()

-- Utils
instance Arbitrary FuncArgs where
    arbitrary = oneof
        [ ArgsVaria <$> arbitrary
        , ArgsArity <$> arbFew arbitrary
        ]