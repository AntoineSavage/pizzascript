module Types.Func.FuncCustom where

import Types.Func.FuncArgs ( FuncArgs )
import Types.Func.FuncImpureArgs ( FuncImpureArgs )
import Types.PzVal ( PzVal )

data FuncCustom
    = FuncCustom FuncImpureArgs FuncArgs PzVal [PzVal]
    deriving (Show, Eq)