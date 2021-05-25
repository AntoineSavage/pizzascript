{-# LANGUAGE LambdaCase #-}
module Utils where

import qualified Data.Set as S

import Data.PzVal ( Dict, PzVal, unparseVal )

unparse :: PzVal -> String
unparse = unparseVal f where
    f Nothing = ""
    f (Just v) = unparseVal f v ++ " "

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

fpure :: Dict -> a -> Result (Dict, a)
fpure ctx r = return (ctx, r)