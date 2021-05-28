module Types.Func.FuncImpureArgs where

import Types.Func.ArgPass ( ArgPass )
import Types.Symb ( Symb )

data FuncImpureArgs
    = None
    | ArgPass ArgPass
    | Both ArgPass Symb
    deriving (Show, Eq)

instance Ord FuncImpureArgs where
    None         <= _            = True
    ArgPass _    <= None         = False
    ArgPass x    <= ArgPass y    = x <= y
    ArgPass _    <= _            = True
    Both _ _     <= None         = False
    Both _ _     <= ArgPass _    = False
    Both apx ecx <= Both apy ecy = case (apx < apy, apx > apy, ecx < ecy, ecx > ecy) of
        (True, _, _, _) -> True
        (_, True, _, _) -> False
        (_, _, True, _) -> True
        (_, _, _, True) -> False
        _               -> True
        
