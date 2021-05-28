module Types.Str where

newtype Str
    = Str String
    deriving (Show)

instance Eq Str where Str x == Str y = x == y
instance Ord Str where Str x <= Str y = x <= y