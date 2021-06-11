module Ops.Func.FuncCustom ( fromFuncCustom, toFuncCustom ) where

import Types.Func ( Func(..) )
import Types.Func.FuncBody ( FuncBody(..) )
import Types.Func.FuncCustom ( FuncCustom(..) )
import Types.Symb ( Symb )
import Types.PzVal ( PzVal, Quoted )

toFuncCustom :: Func (PzVal Quoted) -> Either Symb FuncCustom
toFuncCustom func =
    case body func of
        BodyBuiltIn s -> Left s
        BodyCustom x xs -> return $ FuncCustom (impArgs func) (args func) x xs

fromFuncCustom :: FuncCustom -> Func (PzVal Quoted)
fromFuncCustom (FuncCustom impArgs args x xs) =
    Func impArgs args $ BodyCustom x xs