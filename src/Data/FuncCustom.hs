module Data.FuncCustom ( FuncCustom(..), fromFuncCustom, toFuncCustom ) where

import Data.ArgPass ( ArgPass )
import Data.AstExpr ( AstExpr )
import Data.FuncArgs ( FuncArgs )
import Data.FuncBody ( FuncBody(..) )
import Data.FuncImpureArgs ( FuncImpureArgs )
import Data.Ident ( Ident )
import Data.WithPos ( WithPos )
import Types ( Dict, Func(Func, body, impArgs, args) )

data FuncCustom
    = FuncCustom FuncImpureArgs FuncArgs [WithPos AstExpr]
    deriving (Show, Eq)

toFuncCustom :: Func -> Either Ident FuncCustom
toFuncCustom func =
    case body func of
        BodyBuiltIn ident -> Left ident
        BodyCustom es -> return $ FuncCustom (impArgs func) (args func) es

fromFuncCustom :: FuncCustom -> Func
fromFuncCustom (FuncCustom impArgs args es) =
    Func impArgs args $ BodyCustom es