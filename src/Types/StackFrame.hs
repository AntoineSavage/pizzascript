module Types.StackFrame where

import Types.Func ( Func )
import Types.PzVal ( Dict, PzVal )
import Types.Symb ( Symb )

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