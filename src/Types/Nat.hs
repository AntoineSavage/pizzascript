module Types.Nat where

data Nat
    = Z
    | S Nat
    deriving (Show, Eq, Ord)