module Data.Func.FuncCustom ( FuncCustom(..), fromFuncCustom, toFuncCustom ) where

import Data.Func ( Func(..) )
import Data.Func.FuncArgs ( FuncArgs )
import Data.Func.FuncBody ( FuncBody(..) )
import Data.Func.FuncImpureArgs ( FuncImpureArgs )
import Data.Symb ( Symb )
import Data.PzVal ( PzVal )

data FuncCustom
    = FuncCustom FuncImpureArgs FuncArgs [PzVal]
    deriving (Show, Eq)

toFuncCustom :: Func PzVal -> Either Symb FuncCustom
toFuncCustom func =
    case body func of
        BodyBuiltIn ident -> Left ident
        BodyCustom xs -> return $ FuncCustom (impArgs func) (args func) xs

fromFuncCustom :: FuncCustom -> Func PzVal
fromFuncCustom (FuncCustom impArgs args xs) =
    Func impArgs args $ BodyCustom xs