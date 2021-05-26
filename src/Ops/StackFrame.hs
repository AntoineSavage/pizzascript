module Ops.StackFrame ( setCtx ) where

import Types.PzVal ( Dict )
import Types.StackFrame ( StackFrame(..) )

setCtx :: Dict -> [StackFrame] -> [StackFrame]
setCtx ctx frames = case frames of
    [] -> []
    StackFrame _ spec : fs -> StackFrame ctx spec :fs