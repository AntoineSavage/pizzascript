module Symbs where

import Data.Symb ( Symb, symb )

-- All the following symbols correspond to quoted identifiers
-- i.e. identifiers that were just parsed
-- They also correspond to single-quoted symbols

-- numbers
-- TODO

-- strings
-- TODO

-- symbols
-- TODO

-- booleans
symbFalse :: Symb
symbFalse = symb "false"

symbTrue :: Symb
symbTrue = symb "true"

symbNot :: Symb
symbNot = symb "not"

symbOr :: Symb
symbOr = symb "or"

symbAnd :: Symb
symbAnd = symb "and"

-- lists
symbList :: Symb
symbList = symb "list"

-- dictionaries
symbDict :: Symb
symbDict = symb "dict"

-- functions
symbFunc :: Symb
symbFunc = symb "func"

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

-- miscellaneous
symbCtx :: Symb
symbCtx = symb "ctx"

symbArgs :: Symb
symbArgs = symb "args"

symbX :: Symb
symbX = symb "x"

symbY :: Symb
symbY = symb "y"