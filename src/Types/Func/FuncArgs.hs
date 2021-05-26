module Types.Func.FuncArgs where

import Types.Symb ( Symb )

data FuncArgs
    = ArgsVaria Symb
    | ArgsArity [Symb]
    deriving (Show, Eq, Ord)