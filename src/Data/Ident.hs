module Data.Ident where

newtype Ident
    = Ident String
    deriving (Show, Eq, Ord)