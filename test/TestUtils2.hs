{-# LANGUAGE LambdaCase #-}
module TestUtils2 where
-- TODO This should be moved to the corresponding modules

import qualified Data.Map as M

import Control.Monad
import Data.ArgPass
import Data.ArgPassSpec
import Data.AstExprSpec
import Data.Func
import Data.FuncSpec
import Data.FuncArgs
import Data.FuncArgsSpec
import Data.FuncBody
import Data.FuncBodySpec
import Data.FuncImpureArgs
import Data.FuncImpureArgsSpec
import Data.IdentSpec
import Data.List
import Data.Numb
import Data.PzVal
import Data.PzValSpec
import Data.Str
import Data.Symb
import Data.WithPos
import Data.WithPosSpec
import TestUtils
import Test.QuickCheck
import Types
import Utils

-- Types and instances
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