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
import Data.WithPos ( WithPos(WithPos, val), Pos )
import Idents ( identList, identDict )
import Text.Parsec.Pos ( newPos )

toForm :: Pos -> LstKind -> [WithPos AstExpr] -> [WithPos AstExpr]
toForm p k =
    let identToExpr ident = WithPos p $ AstIdent ident
    in case k of
        KindList -> (identToExpr identList:)
        KindDict -> (identToExpr identDict:)
        KindForm -> id

getIdent :: WithPos AstExpr -> Either String (WithPos Ident)
getIdent (WithPos p v) = case v of
    AstIdent ident -> return $ WithPos p ident
    _ -> Left $ "Expected identifier"
        ++ "\n was: " ++ show v
        ++ "\n at: " ++ show p
getDuplicates :: Ord a => [a] -> [a]
getDuplicates = go S.empty S.empty where
    go s dups []     = S.toList dups
    go s dups (x:xs) = if S.member x s
        then go s (S.insert x dups) xs
        else go (S.insert x s) dups xs

addIdentAndPos :: Pos -> Maybe (WithPos Ident) -> String -> String
addIdentAndPos p Nothing s  = s ++ "\n at:" ++ show p
addIdentAndPos p (Just fi) s = s ++ "\n at " ++ show fi ++ ": " ++ show p

invalidArityMsg :: Int -> [a] -> String
invalidArityMsg n args = "Invalid number of arguments. Expected " ++ show n ++ ", got: " ++ show (length args)

f0 :: [a] -> (() -> Either String b) -> Either String b
f0 args f = case args of [] -> f (); _ -> Left $ invalidArityMsg 0 args

f1 :: [a] -> (a -> Either String b) -> Either String b
f1 args f = case args of [x] -> f x; _ -> Left $ invalidArityMsg 1 args

f2 :: [a] -> (a -> a -> Either String b) -> Either String b
f2 args f = case args of [x, y] -> f x y; _ -> Left $ invalidArityMsg 2 args

f3 :: [a] -> (a -> a -> a -> Either String b) -> Either String b
f3 args f = case args of [x, y, z] -> f x y z; _ -> Left $ invalidArityMsg 3 args

fpure :: Dict -> a -> Either String (Dict, a)
fpure ctx r = return (ctx, r)