module Reduce where

import Types.PzVal ( PzVal )
import Types.Symb ( Symb(..) )
import Types.StackFrame ( StackFrame )
import Utils ( Result )

type AccResult = Result Acc
type ReturnValue = Maybe PzVal
data Acc
    = Acc ReturnValue [StackFrame]
    deriving (Show, Eq)