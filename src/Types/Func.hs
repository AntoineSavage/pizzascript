module Types.Func where

import Types.Func.FuncArgs ( FuncArgs )
import Types.Func.FuncBody ( FuncBody )
import Types.Func.FuncImpureArgs ( FuncImpureArgs )

data Func a
    = Func { impArgs :: FuncImpureArgs, args :: FuncArgs, body :: FuncBody a }
    deriving (Show, Eq, Ord)