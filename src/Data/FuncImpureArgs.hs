module Data.FuncImpureArgs where

import Data.ArgPass ( ArgPass )
import Data.Ident ( Ident )
import Data.WithPos ( Pos, WithPos )

data FuncImpureArgs
    = None
    | ArgPass Pos (WithPos ArgPass)
    | Both Pos (WithPos ArgPass) (WithPos Ident)
    deriving (Show, Eq, Ord)