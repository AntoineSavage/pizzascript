module Types.Str where

newtype Str
    = Str String
    deriving (Show, Eq)

instance Ord Str where Str x <= Str y = x <= y