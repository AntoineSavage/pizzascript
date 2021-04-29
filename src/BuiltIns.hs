module BuiltIns where

import qualified Data.Map as M

import Data.Nat ( Nat(..) )
import Types

ident :: String -> Ident
ident = Ident . (:[])

symb :: String -> Symb
symb = fromIdent . ident

fromIdent :: Ident -> Symb
fromIdent = Symb Z

toSymb :: FuncArgPass -> Symb
toSymb Eval        = symbEval
toSymb Quote       = symbQuote
toSymb Unquote     = symbUnquote
toSymb DeepQuote   = symbDeepQuote
toSymb DeepUnquote = symbDeepUnquote

fromSymb :: Symb -> Maybe FuncArgPass
fromSymb symb =
    let m = M.fromList
            [ (symbEval, Eval)
            , (symbQuote, Quote)
            , (symbUnquote, Unquote)
            , (symbDeepQuote, DeepQuote)
            , (symbDeepUnquote, DeepUnquote)
            ]
    in  M.lookup symb m

toForm :: AstPos -> AstListKind -> [AstExpr] -> [AstExpr]
toForm p k =
    let identToExpr ident = AstExpr p "" $ AstIdent ident
    in case k of
        KindList -> (identToExpr identList:)
        KindDict -> (identToExpr identDict:)
        KindForm -> id

identList :: Ident
identList = ident "list"

identDict :: Ident
identDict = ident "dict"

identFunc :: Ident
identFunc = ident "func"

symbTrue :: Symb
symbTrue = symb "true"

symbFalse :: Symb
symbFalse = symb "false"

symbArgs :: Symb
symbArgs = symb "args"

symbEval :: Symb
symbEval = symb "eval"

symbQuote :: Symb
symbQuote = symb "quote"

symbUnquote :: Symb
symbUnquote = symb "unquote"

symbDeepQuote :: Symb
symbDeepQuote = symb "deep_quote"

symbDeepUnquote :: Symb
symbDeepUnquote = symb "deep_unquote"

argsVariadic :: FuncArgs
argsVariadic = ArgsVaria symbArgs