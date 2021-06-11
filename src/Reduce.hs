module Reduce where

import Types.PzVal ( Evaled, PzVal )
import Types.StackFrame ( StackFrame )
import Utils ( Result )

type AccResult = Result Acc
type ReturnValue = Maybe (PzVal Evaled)
data Acc
    = Acc ReturnValue [StackFrame]
    deriving (Show, Eq)