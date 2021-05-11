module Data.Symb where

import Data.Ident ( Ident )
import Data.Nat ( Nat )

data Symb
    = Symb Nat Ident
    deriving (Show, Eq, Ord)
