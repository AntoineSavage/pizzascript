module Data.Func.FuncBody where

import Data.AstExpr ( AstExpr )
import Data.Ident ( Ident )

data FuncBody
    = BodyBuiltIn Ident
    | BodyCustom [AstExpr]
    deriving (Show, Eq, Ord)