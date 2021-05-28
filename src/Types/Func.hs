module Types.Func where

import Types.Func.FuncArgs ( FuncArgs )
import Types.Func.FuncBody ( FuncBody )
import Types.Func.FuncImpureArgs ( FuncImpureArgs )

data Func a
    = Func { impArgs :: FuncImpureArgs, args :: FuncArgs, body :: FuncBody a }
    deriving (Show, Eq)

instance Ord a => Ord (Func a) where
    Func iax ax bx <= Func iay ay by = case (iax < iay, iax > iay, ax < ay, ax > ay, bx < by, bx > by) of
        (True, _, _, _, _, _) -> True
        (_, True, _, _, _, _) -> False
        (_, _, True, _, _, _) -> True
        (_, _, _, True, _, _) -> False
        (_, _, _, _, True, _) -> True
        (_, _, _, _, _, True) -> False
        _                     -> True

