module Types.Func.FuncBody where

import Types.Symb ( Symb )

data FuncBody a
    = BodyBuiltIn Symb
    | BodyCustom a [a]
    deriving (Show, Eq)

instance Ord a => Ord (FuncBody a) where
    BodyBuiltIn x   <= BodyBuiltIn y   = x <= y
    BodyBuiltIn _   <= _               = True
    BodyCustom _ _  <= BodyBuiltIn _   = False
    BodyCustom x xs <= BodyCustom y ys = case (x < y, x > y, xs < ys, xs > ys) of
        (True, _, _, _) -> True
        (_, True, _, _) -> False
        (_, _, True, _) -> True
        (_, _, _, True) -> False
        _               -> True