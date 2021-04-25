module Pz.PzDict (PzDict(..), eval, uneval) where

data PzDict
    = PzDict
    deriving (Show, Eq)

eval :: a -> PzDict
eval = undefined

uneval :: PzDict -> a
uneval = undefined