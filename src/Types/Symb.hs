module Types.Symb where

import Types.Nat ( Nat )

-- Symbols are implicitely quoted once
-- i.e. n=Z corresponds to one quote
data Symb
    = Symb Nat Char String
    deriving (Show, Eq, Ord)