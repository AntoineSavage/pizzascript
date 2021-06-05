{-# LANGUAGE LambdaCase #-}
module Ops.Func.FuncImpureArgs ( getArgPass, getExplCtx ) where

import Types.Func.ArgPass ( ArgPass(Eval) )
import Types.Func.FuncImpureArgs ( FuncImpureArgs(..) )
import Types.Symb ( Symb )

getArgPass :: FuncImpureArgs -> ArgPass
getArgPass = \case
    None -> Eval
    ArgPass ap -> ap
    Both ap _ -> ap

getExplCtx :: FuncImpureArgs -> Maybe Symb
getExplCtx = \case
    Both _ ec -> return ec
    _         -> Nothing