module Types.StackFrame where

import Types.Func ( Func )
import Types.PzVal ( Evaled, Dict, PzFunc, PzVal, Quoted )
import Types.Symb ( Symb )

data StackFrame
    = StackFrame Dict StackFrameSpec
    deriving (Show, Eq)

type Stmts = [PzVal Quoted]

type FuncSymb = Maybe Symb
type FuncArg = PzVal Evaled
type RemArgs = [PzVal Quoted]

type ImplCtx = Dict
type InvocFunc = PzFunc
type RdyArgs a = [PzVal a]

data StackFrameSpec
    = Block Stmts
    | Form FuncSymb FuncArg RemArgs
    | InvocQuoted (Invoc Quoted)
    | InvocEvaled (Invoc Evaled)
    deriving (Show, Eq)

data Invoc a
    = Invoc FuncSymb ImplCtx InvocFunc (RdyArgs a) (Maybe RemArgs)
    deriving (Show, Eq)