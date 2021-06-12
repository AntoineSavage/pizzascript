module Types.StackFrame where

import Types.Func ( Func )
import Types.PzVal ( Evaled, Dict, PzFunc, PzVal, Quoted )
import Types.Symb ( Symb )

data StackFrame
    = StackFrame Dict StackFrameSpec
    deriving (Show, Eq)

type FuncSymb = Maybe Symb
type QArgs = [PzVal Quoted]

data StackFrameSpec
    = Block [PzVal Quoted]
    | FormQuoted FuncSymb (PzVal Quoted) QArgs
    | FormEvaled FuncSymb (PzVal Evaled) QArgs
    | InvocQuoted FuncSymb Dict PzFunc QArgs
    | InvocEvaled FuncSymb Dict PzFunc [PzVal Evaled] (Maybe QArgs)
    deriving (Show, Eq)