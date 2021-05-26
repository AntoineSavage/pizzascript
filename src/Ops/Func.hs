module Ops.Func ( getArgPass ) where

import Types.Func ( Func(..) )
import Types.Func.ArgPass ( ArgPass(..) )
import Types.Func.FuncImpureArgs ( FuncImpureArgs(..) )

getArgPass :: Func a -> ArgPass
getArgPass func = case impArgs func of
    None -> Eval
    ArgPass ap -> ap
    Both ap _ -> ap