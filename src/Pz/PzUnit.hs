module Pz.PzUnit (PzUnit(..), eval, uneval) where

data PzUnit
    = PzUnit
    deriving (Show, Eq)

eval :: a -> PzUnit
eval = undefined

uneval :: PzUnit -> a
uneval = undefined