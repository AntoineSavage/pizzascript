module Utils where

import qualified Data.Map as M

import Data.Maybe ( fromMaybe )
import Data.Nat ( Nat(..) )
import Types
import Text.Parsec.Pos ( newPos )

ident :: String -> Ident
ident = Ident . (:[])

symb :: Ident -> Symb
symb = Symb Z

toForm :: AstPos -> AstListKind -> [AstExpr] -> [AstExpr]
toForm p k =
    let identToExpr ident = AstExpr p $ AstIdent ident
    in case k of
        KindList -> (identToExpr identList:)
        KindDict -> (identToExpr identDict:)
        KindForm -> id

mapBoth :: (a -> b -> (c, d)) -> [(a, b)] -> [(c, d)]
mapBoth f = map $ uncurry f

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