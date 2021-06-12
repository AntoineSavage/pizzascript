module Types.StackFrame where

import Types.Func ( Func )
import Types.PzVal ( Evaled, Dict, PzFunc, PzVal, Quoted )
import Types.Symb ( Symb )

data StackFrame
    = StackFrame Dict StackFrameSpec
    deriving (Show, Eq)

type QArgs = [PzVal Quoted]

data StackFrameSpec
    = Block [PzVal Quoted]
    | FormQuoted (PzVal Quoted) QArgs
    | FormEvaled (PzVal Evaled) QArgs
    | InvocQuoted Dict PzFunc QArgs
    | InvocEvaled Dict PzFunc [PzVal Evaled] (Maybe QArgs)
    deriving (Show, Eq)