module Data.Func.FuncBody where

import Data.Symb ( Symb )

data FuncBody a
    = BodyBuiltIn Symb
    | BodyCustom [a]
    deriving (Show, Eq, Ord)