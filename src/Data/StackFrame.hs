module Data.StackFrame ( StackFrame(..), StackFrameSpec(..), setCtx ) where

import Data.Func ( Func )
import Data.PzVal ( Dict, PzVal )
import Data.Symb ( Symb )

data StackFrame
    = StackFrame Dict StackFrameSpec
    deriving (Show, Eq)

type Stmts = [PzVal]

type FuncSymb = Maybe Symb
type FuncArg = PzVal
type RemArgs = [PzVal]

type ImplCtx = Dict
type InvocFunc = Func PzVal
type RdyArgs = [PzVal]

data StackFrameSpec
    = Block Stmts
    | Form FuncSymb FuncArg RemArgs
    | Invoc FuncSymb ImplCtx InvocFunc RdyArgs (Maybe RemArgs)
    deriving (Show, Eq)

setCtx :: Dict -> [StackFrame] -> [StackFrame]
setCtx ctx frames = case frames of
    [] -> []
    StackFrame _ spec : fs -> StackFrame ctx spec :fs