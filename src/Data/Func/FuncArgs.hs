module Data.Func.FuncArgs where

import Data.Symb ( Symb )

data FuncArgs
    = ArgsVaria Symb
    | ArgsArity [Symb]
    deriving (Show, Eq, Ord)