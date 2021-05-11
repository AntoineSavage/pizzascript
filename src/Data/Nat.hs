module Data.Nat ( Nat(..), len, unlen ) where

data Nat
    = Z
    | S Nat
    deriving (Show, Eq, Ord)

len :: [a] -> Nat
len []     = Z
len (_:xs) = S $ len xs

unlen :: Nat -> a -> [a]
unlen Z     _ = []
unlen (S n) x = x : unlen n x