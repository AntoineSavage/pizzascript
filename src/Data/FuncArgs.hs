module Data.FuncArgs where

import Data.AstExpr ( AstExpr )
import Data.Ident ( Ident )
import Data.WithPos ( Pos, WithPos )

data FuncArgs
    = ArgsVaria (WithPos Ident)
    | ArgsArity Pos [WithPos Ident]
    deriving (Show, Eq, Ord)