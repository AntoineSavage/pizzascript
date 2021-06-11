module Types.Func.FuncCustom where

import Types.Func.FuncArgs ( FuncArgs )
import Types.Func.FuncImpureArgs ( FuncImpureArgs )
import Types.PzVal ( PzVal, Quoted )

data FuncCustom
    = FuncCustom FuncImpureArgs FuncArgs (PzVal Quoted) [PzVal Quoted]
    deriving (Show, Eq)