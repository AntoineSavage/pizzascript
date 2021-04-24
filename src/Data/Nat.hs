module Data.Nat (Nat(..), len, unlen) where

data Nat
    = Z
    | S Nat
    deriving (Show, Eq)

len :: [a] -> Nat
len []     = Z
len (_:xs) = S $ len xs

unlen :: a -> Nat -> [a]
unlen _ Z     = []
unlen x (S n) = x : unlen x n 