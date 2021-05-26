module Ops.Func.FuncArgsSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Ops.SymbSpec
import TestUtils
import Types.Func.FuncArgs

spec :: Spec
spec = return ()

-- Utils
instance Arbitrary FuncArgs where
    arbitrary = oneof
        [ ArgsVaria <$> arbQuotedIdent
        , ArgsArity <$> arbFew arbQuotedIdent
        ]