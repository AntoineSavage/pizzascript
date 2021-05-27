module Types.Numb where

newtype Numb
    = Numb Double
    deriving (Show, Eq)

instance Ord Numb where Numb x <= Numb y = x <= y