module Utils where

import qualified Data.Set as S

import Ops.PzVal ( unparseVal )
import Types.PzVal ( Dict, PzVal, Quoted )

unparse :: PzVal Quoted -> String
unparse = unparseVal f where
    f v last  = unparseVal f v ++ if last then "" else " "

getDuplicates :: Ord a => [a] -> [a]
getDuplicates = go S.empty S.empty where
    go s dups []     = S.toList dups
    go s dups (x:xs) = if S.member x s
        then go s (S.insert x dups) xs
        else go (S.insert x s) dups xs

invalidArityMsg :: Int -> [a] -> String
invalidArityMsg n args = "Invalid number of arguments. Expected " ++ show n ++ ", got: " ++ show (length args)

type Result = Either String

f0 :: [a] -> (() -> Result b) -> Result b
f0 args f = case args of [] -> f (); _ -> Left $ invalidArityMsg 0 args

f1 :: [a] -> (a -> Result b) -> Result b
f1 args f = case args of [x] -> f x; _ -> Left $ invalidArityMsg 1 args

f2 :: [a] -> (a -> a -> Result b) -> Result b
f2 args f = case args of [x, y] -> f x y; _ -> Left $ invalidArityMsg 2 args

f3 :: [a] -> (a -> a -> a -> Result b) -> Result b
f3 args f = case args of [x, y, z] -> f x y z; _ -> Left $ invalidArityMsg 3 args