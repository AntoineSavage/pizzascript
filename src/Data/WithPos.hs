module Data.WithPos ( Pos, WithPos(..) ) where

import Text.Parsec ( SourcePos )

type Pos = SourcePos
data WithPos a
    = WithPos { pos :: Pos, val :: a }

-- Ignore position in show, eq and ord
instance Show a => Show (WithPos a) where show (WithPos _ x) = show x
instance Eq a => Eq (WithPos a) where (==) (WithPos _ x) (WithPos _ y) = x == y
instance Ord a => Ord (WithPos a) where compare (WithPos _ x) (WithPos _ y) = compare x y
instance Functor WithPos where fmap f (WithPos p x) = WithPos p $ f x