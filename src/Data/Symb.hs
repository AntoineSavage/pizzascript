module Data.Symb where

import Data.Ident ( Ident (Ident), ident )
import Data.Nat ( Nat(Z) )

data Symb
    = Symb Nat Ident
    deriving (Show, Eq, Ord)

fromIdent :: Ident -> Symb
fromIdent = Symb Z

symb :: String -> Symb
symb = fromIdent . ident

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