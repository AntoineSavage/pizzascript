module Ops.Nat ( len, unlen ) where

import Types.Nat ( Nat(..) )

len :: [a] -> Nat
len []     = Z
len (_:xs) = S $ len xs

unlen :: Nat -> a -> [a]
unlen Z     _ = []
unlen (S n) x = x : unlen n x