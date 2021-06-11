module Types.StackFrame where

import Types.Func ( Func )
import Types.PzVal ( Evaled, Dict, PzVal, Quoted )
import Types.Symb ( Symb )

data StackFrame
    = StackFrame Dict StackFrameSpec
    deriving (Show, Eq)

type Stmts = [PzVal Quoted]

type FuncSymb = Maybe Symb
type FuncArg = PzVal Evaled
type RemArgs = [PzVal Quoted]

type ImplCtx = Dict
type InvocFunc = Func (PzVal Quoted)
type RdyArgs = [PzVal Evaled]

data StackFrameSpec
    = Block Stmts
    | Form FuncSymb FuncArg RemArgs
    | Invoc FuncSymb ImplCtx InvocFunc RdyArgs (Maybe RemArgs)
    deriving (Show, Eq)