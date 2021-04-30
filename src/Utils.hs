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
    let identToExpr ident = AstExpr p "" $ AstIdent ident
    in case k of
        KindList -> (identToExpr identList:)
        KindDict -> (identToExpr identDict:)
        KindForm -> id

boolish :: PzVal -> Bool
boolish PzUnit = False
boolish (PzNum 0) = False
boolish (PzStr "") = False
boolish (PzSymb (Symb Z ident)) = ident /= identFalse
boolish (PzList []) = False
boolish (PzDict d) = not $ M.null d
boolish _ = True

type FuncReturn = Either String (Dict, PzVal)

invalidArityMsg :: Int -> [a] -> String
invalidArityMsg n args = "Invalid number of arguments. Expected " ++ show n ++ ", got: " ++ show (length args)

f0 :: [PzVal] -> (() -> FuncReturn) -> FuncReturn
f0 args f = case args of [] -> f (); _ -> Left $ invalidArityMsg 0 args

f1 :: [PzVal] -> (PzVal -> FuncReturn) -> FuncReturn
f1 args f = case args of [x] -> f x; _ -> Left $ invalidArityMsg 1 args

f2 :: [PzVal] -> (PzVal -> PzVal -> FuncReturn) -> FuncReturn
f2 args f = case args of [x, y] -> f x y; _ -> Left $ invalidArityMsg 2 args

f3 :: [PzVal] -> (PzVal -> PzVal -> PzVal -> FuncReturn) -> FuncReturn
f3 args f = case args of [x, y, z] -> f x y z; _ -> Left $ invalidArityMsg 3 args

dictGet :: PzVal -> Dict -> PzVal
dictGet k m = fromMaybe PzUnit $ M.lookup k m

-- misc
pos :: AstPos
pos = newPos "<built-ins>" 0 0

identList :: Ident
identList = ident "list"

identDict :: Ident
identDict = ident "dict"

identFunc :: Ident
identFunc = ident "func"

identFalse :: Ident
identFalse = ident "false"

identTrue :: Ident
identTrue = ident "true"

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