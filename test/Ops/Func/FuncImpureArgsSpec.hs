module Ops.Func.FuncImpureArgsSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Ops.Func.ArgPassSpec
import Ops.SymbSpec
import Types.Func.FuncImpureArgs

spec :: Spec
spec = return ()

-- Utils
instance Arbitrary FuncImpureArgs where
    arbitrary = oneof
        [ return None
        , ArgPass <$> arbitrary
        , liftM2 Both arbitrary arbQuotedIdent
        ]