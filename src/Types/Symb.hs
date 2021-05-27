module Types.Symb where

import Types.Nat ( Nat )

-- Symbols are implicitely quoted once
-- i.e. n=Z corresponds to one quote
data Symb
    = Symb Nat Char String
    deriving (Show, Eq)

instance Ord Symb where
    Symb nx cx sx <= Symb ny cy sy =
        case (nx < ny, nx > ny, cx < cy, cx > cy, sx < sy, sx > sy) of
            (True, _, _, _, _, _) -> True
            (_, True, _, _, _, _) -> False
            (_, _, True, _, _, _) -> True
            (_, _, _, True, _, _) -> False
            (_, _, _, _, True, _) -> True
            (_, _, _, _, _, True) -> False
            _                     -> True