module Types.Func.FuncArgs where

import Types.Symb ( Symb )

data FuncArgs
    = ArgsVaria Symb
    | ArgsArity [Symb]
    deriving (Show, Eq)

instance Ord FuncArgs where
    ArgsVaria x <= ArgsVaria y = x <= y
    ArgsVaria _ <= ArgsArity _ = True
    ArgsArity _ <= ArgsVaria _ = False
    ArgsArity x <= ArgsArity y = x <= y