{-# LANGUAGE LambdaCase #-}
module Utils where

import qualified Data.Map as M
import qualified Data.Set as S

import Data.Maybe ( fromMaybe )
import Data.Nat ( Nat(..) )
import Types
import Text.Parsec.Pos ( newPos )

ident :: String -> Ident
ident = Ident . (:[])

symb :: Ident -> Symb
symb = Symb Z

argPassToSymb :: ArgPass -> Symb
argPassToSymb = \case
    Eval -> symbEval
    Quote -> symbQuote
    Unquote -> symbUnquote
    DeepQuote -> symbDeepQuote
    DeepUnquote -> symbDeepUnquote

symbToArgPass :: Symb -> Maybe ArgPass
symbToArgPass = flip M.lookup m where
    m = M.fromList
        [ (symbEval, Eval)
        , (symbQuote, Quote)
        , (symbUnquote, Unquote)
        , (symbDeepQuote, DeepQuote)
        , (symbDeepUnquote, DeepUnquote)
        ]

toForm :: Pos -> AstListKind -> [WithPos AstExpr] -> [WithPos AstExpr]
toForm p k =
    let identToExpr ident = WithPos p $ AstIdent ident
    in case k of
        KindList -> (identToExpr identList:)
        KindDict -> (identToExpr identDict:)
        KindForm -> id

getIdent :: WithPos AstExpr -> Maybe (WithPos Ident)
getIdent (WithPos p v) = case v of
    AstIdent ident -> return $ WithPos p ident
    _ -> Nothing

getArgPass :: Func -> ArgPass
getArgPass func = case impArgs func of
    None -> Eval 
    ArgPass _ ap -> val ap
    Both _ ap _ -> val ap

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

----------------------
-- Custom functions
----------------------
data FuncCustom
    = FuncCustom FuncImpureArgs FuncArgs [WithPos AstExpr]
    deriving (Show, Eq)

toFuncCustom :: Func -> Either Ident FuncCustom
toFuncCustom func =
    case body func of
        BodyBuiltIn ident -> Left ident
        BodyCustom es -> return $ FuncCustom (impArgs func) (args func) es

fromFuncCustom :: Dict -> FuncCustom -> Func
fromFuncCustom implCtx (FuncCustom impArgs args es) =
    Func implCtx impArgs args $ BodyCustom es

-----------------
-- Identifiers
-----------------

-- numbers
-- TODO

-- strings
-- TODO

-- symbols
-- TODO

-- booleans
identFalse :: Ident
identFalse = ident "false"

identTrue :: Ident
identTrue = ident "true"

identNot :: Ident
identNot = ident "not"

identOr :: Ident
identOr = ident "or"

identAnd :: Ident
identAnd = ident "and"

-- lists
identList :: Ident
identList = ident "list"

-- dictionaries
identDict :: Ident
identDict = ident "dict"

-- functions
identFunc :: Ident
identFunc = ident "func"

-- miscellaneous
identCtx :: Ident
identCtx = ident "ctx"

identArgs :: Ident
identArgs = ident "args"

identX :: Ident
identX = ident "x"

identY :: Ident
identY = ident "y"

-------------
-- Symbols
-------------

-- numbers
-- TODO

-- strings
-- TODO

-- symbols
-- TODO

-- booleans
symbFalse :: Symb
symbFalse = symb identFalse

symbTrue :: Symb
symbTrue = symb identTrue

-- lists
-- TODO

-- dictionaries
-- TODO

-- functions
symbEval :: Symb
symbEval = symb $ ident "eval"

symbQuote :: Symb
symbQuote = symb $ ident "quote"

symbUnquote :: Symb
symbUnquote = symb $ ident "unquote"

symbDeepQuote :: Symb
symbDeepQuote = symb $ ident "deep_quote"

symbDeepUnquote :: Symb
symbDeepUnquote = symb $ ident "deep_unquote"

-- miscellaneous
-- TODO