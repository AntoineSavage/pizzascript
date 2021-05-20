module Symbs where

import Data.Ident ( Ident(Ident) )
import Data.Symb ( Symb, symb )
import Idents ( identFalse, identTrue )

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
symbEval = symb $ Ident "eval"

symbQuote :: Symb
symbQuote = symb $ Ident "quote"

symbUnquote :: Symb
symbUnquote = symb $ Ident "unquote"

symbDeepQuote :: Symb
symbDeepQuote = symb $ Ident "deep_quote"

symbDeepUnquote :: Symb
symbDeepUnquote = symb $ Ident "deep_unquote"

-- miscellaneous
-- TODO