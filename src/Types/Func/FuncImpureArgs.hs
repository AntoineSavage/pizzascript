module Types.Func.FuncImpureArgs where

import Types.Func.ArgPass ( ArgPass )
import Types.Symb ( Symb )

data FuncImpureArgs
    = None
    | ArgPass ArgPass
    | Both ArgPass Symb
    deriving (Show, Eq, Ord)