module Data.Func.FuncCustom ( FuncCustom(..), fromFuncCustom, toFuncCustom ) where

import Data.AstExpr ( AstExpr )
import Data.Func ( Func(..) )
import Data.Func.ArgPass ( ArgPass )
import Data.Func.FuncArgs ( FuncArgs )
import Data.Func.FuncBody ( FuncBody(..) )
import Data.Func.FuncImpureArgs ( FuncImpureArgs )
import Data.Ident ( Ident )

data FuncCustom
    = FuncCustom FuncImpureArgs FuncArgs [AstExpr]
    deriving (Show, Eq)

toFuncCustom :: Func -> Either Ident FuncCustom
toFuncCustom func =
    case body func of
        BodyBuiltIn ident -> Left ident
        BodyCustom es -> return $ FuncCustom (impArgs func) (args func) es

fromFuncCustom :: FuncCustom -> Func
fromFuncCustom (FuncCustom impArgs args es) =
    Func impArgs args $ BodyCustom es