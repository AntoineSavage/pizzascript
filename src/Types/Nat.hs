module Types.Nat where

data Nat
    = Z
    | S Nat
    deriving (Show, Eq)

instance Ord Nat where
    Z   <= _   = True
    S _ <= Z   = False
    S x <= S y = x <= y