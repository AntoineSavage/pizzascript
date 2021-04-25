module Pz.PzList (PzList(..), eval, uneval) where

data PzList
    = PzList
    deriving (Show, Eq)

eval :: a -> PzList
eval = undefined

uneval :: PzList -> a
uneval = undefined