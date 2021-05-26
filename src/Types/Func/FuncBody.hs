module Types.Func.FuncBody where

import Types.Symb ( Symb )

data FuncBody a
    = BodyBuiltIn Symb
    | BodyCustom a [a]
    deriving (Show, Eq, Ord)