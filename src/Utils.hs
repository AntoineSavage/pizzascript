{-# LANGUAGE LambdaCase #-}
module Utils where

import qualified Data.Map as M
import qualified Data.Set as S

import Data.AstExpr ( AstExpr(..) )
import Data.Func ( Func(..) )
import Data.Func.ArgPass ( ArgPass(..) )
import Data.Func.FuncImpureArgs ( FuncImpureArgs(..) )
import Data.Ident ( Ident(..) )
import Data.Lst ( LstKind(..) )
import Data.PzVal ( Dict )
import Idents ( identList, identDict )
import Text.Parsec.Pos ( newPos )

type Result = Either String

toForm :: LstKind -> [AstExpr] -> [AstExpr]
toForm k =
    case k of
        KindList -> (AstIdent identList:)
        KindDict -> (AstIdent identDict:)
        KindForm -> id

getIdent :: AstExpr -> Result Ident
getIdent e = case e of
    AstIdent ident -> return ident
    _ -> Left $ "Expected identifier"
        ++ "\n was: " ++ show e

getDuplicates :: Ord a => [a] -> [a]
getDuplicates = go S.empty S.empty where
    go s dups []     = S.toList dups
    go s dups (x:xs) = if S.member x s
        then go s (S.insert x dups) xs
        else go (S.insert x s) dups xs

addIdentAndPos :: Maybe Ident -> String -> String
addIdentAndPos Nothing s  = s
addIdentAndPos (Just fi) s = s ++ "\n at " ++ show fi

invalidArityMsg :: Int -> [a] -> String
invalidArityMsg n args = "Invalid number of arguments. Expected " ++ show n ++ ", got: " ++ show (length args)

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