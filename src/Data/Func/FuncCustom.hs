module Data.Func.FuncCustom ( FuncCustom(..), fromFuncCustom, toFuncCustom ) where

import Data.Func ( Func(..) )
import Data.Func.FuncArgs ( FuncArgs )
import Data.Func.FuncBody ( FuncBody(..) )
import Data.Func.FuncImpureArgs ( FuncImpureArgs )
import Data.Symb ( Symb )
import Data.PzVal ( PzVal )

data FuncCustom
    = FuncCustom FuncImpureArgs FuncArgs PzVal [PzVal]
    deriving (Show, Eq)

toFuncCustom :: Func PzVal -> Either Symb FuncCustom
toFuncCustom func =
    case body func of
        BodyBuiltIn s -> Left s
        BodyCustom x xs -> return $ FuncCustom (impArgs func) (args func) x xs

fromFuncCustom :: FuncCustom -> Func PzVal
fromFuncCustom (FuncCustom impArgs args x xs) =
    Func impArgs args $ BodyCustom x xs