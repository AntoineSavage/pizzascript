module Data.Ident (Ident(..), ident) where

newtype Ident
    = Ident [String]
    deriving (Show, Eq, Ord)

ident :: String -> Ident
ident = Ident . (:[])