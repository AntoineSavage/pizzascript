module Pz.PzFunc (PzFunc(..), eval, uneval) where

data PzFunc
    = PzFunc
    deriving (Show, Eq)

eval :: a -> PzFunc
eval = undefined

uneval :: PzFunc -> a
uneval = undefined