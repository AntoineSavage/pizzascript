module BuiltIns where

import qualified Data.Map as M

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

pos :: AstPos
pos = newPos "<built-ins>" 0 0

identArgs :: Ident
identArgs = ident "args"

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

-- built-in constants
false :: PzVal
false = PzSymb $ symb identFalse

true :: PzVal
true = PzSymb $ symb identTrue

-- built-in functions
-- TODO

{- TODO for base:
    - not, or, and
    - empty, size (list / dict)
    - cons, head, tail
    - keys, get, put, del
    - def
    - func
-}

-- Built-in context
builtInCtx :: Dict
builtInCtx = M.fromList
    [ (false, false)
    , (true, true)
    ]