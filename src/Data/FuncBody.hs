module Data.FuncBody where

import Data.AstExpr ( AstExpr )
import Data.Ident ( Ident )
import Data.WithPos ( WithPos )

data FuncBody
    = BodyBuiltIn Ident
    | BodyCustom [WithPos AstExpr]
    deriving (Show, Eq, Ord)