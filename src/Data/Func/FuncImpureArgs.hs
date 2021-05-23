module Data.Func.FuncImpureArgs where

import Data.Func.ArgPass ( ArgPass )
import Data.Ident ( Ident )

data FuncImpureArgs
    = None
    | ArgPass ArgPass
    | Both ArgPass Ident
    deriving (Show, Eq, Ord)