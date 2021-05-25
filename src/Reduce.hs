module Reduce where

import BuiltIns.Impls
import Data.PzVal ( Dict, PzVal )
import Data.Symb ( Symb(..) )
import Data.Nat
import Data.StackFrame
import Utils

type AccResult = Result Acc
type ReturnValue = Maybe PzVal
data Acc
    = Acc ReturnValue [StackFrame]
    deriving (Show, Eq)