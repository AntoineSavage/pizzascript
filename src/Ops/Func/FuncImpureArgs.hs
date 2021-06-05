{-# LANGUAGE LambdaCase #-}
module Ops.Func.FuncImpureArgs ( getArgPass ) where

import Types.Func.ArgPass ( ArgPass(Eval) )
import Types.Func.FuncImpureArgs ( FuncImpureArgs(..) )

getArgPass :: FuncImpureArgs -> ArgPass
getArgPass = \case
    None -> Eval
    ArgPass ap -> ap
    Both ap _ -> ap