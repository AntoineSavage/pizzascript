module Types.Numb where

newtype Numb
    = Numb Double
    deriving (Show)

instance Eq Numb where Numb x == Numb y = x == y
instance Ord Numb where Numb x <= Numb y = x <= y