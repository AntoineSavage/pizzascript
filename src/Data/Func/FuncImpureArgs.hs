module Data.Func.FuncImpureArgs where

import Data.Func.ArgPass ( ArgPass )
import Data.Symb ( Symb )

data FuncImpureArgs
    = None
    | ArgPass ArgPass
    | Both ArgPass Symb
    deriving (Show, Eq, Ord)