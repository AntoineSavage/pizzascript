module Data.FuncCustom ( FuncCustom(..), fromFuncCustom, toFuncCustom ) where

import Data.AstExpr ( AstExpr )
import Data.Ident ( Ident )
import Data.WithPos ( WithPos )
import Types

data FuncCustom
    = FuncCustom FuncImpureArgs FuncArgs [WithPos AstExpr]
    deriving (Show, Eq)

toFuncCustom :: Func -> Either Ident FuncCustom
toFuncCustom func =
    case body func of
        BodyBuiltIn ident -> Left ident
        BodyCustom es -> return $ FuncCustom (impArgs func) (args func) es

fromFuncCustom :: Dict -> FuncCustom -> Func
fromFuncCustom implCtx (FuncCustom impArgs args es) =
    Func implCtx impArgs args $ BodyCustom es