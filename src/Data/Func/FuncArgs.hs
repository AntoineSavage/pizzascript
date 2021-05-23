module Data.Func.FuncArgs where

import Data.AstExpr ( AstExpr )
import Data.Ident ( Ident )

data FuncArgs
    = ArgsVaria Ident
    | ArgsArity [Ident]
    deriving (Show, Eq, Ord)