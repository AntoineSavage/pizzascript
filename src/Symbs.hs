module Symbs where

import Ops.Symb ( quoteSymb, symb )
import Types.Nat ( Nat(..) ) 
import Types.Symb ( Symb(..) )

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


-- miscellaneous
symbCtx :: Symb
symbCtx = symb "ctx"

symbArgs :: Symb
symbArgs = symb "args"

symbX :: Symb
symbX = symb "x"

symbY :: Symb
symbY = symb "y"

-- All the following symbols correspond to non-quoted identifiers
-- i.e. symbols that were just parsed
-- They also correspond to one-or-more-quoted symbols
symbEval :: Symb
symbEval = quoteSymb $ symb "eval"

symbQuote :: Symb
symbQuote = quoteSymb $ symb "quote"

symbUnquote :: Symb
symbUnquote = quoteSymb $ symb "unquote"

symbDeepQuote :: Symb
symbDeepQuote = quoteSymb $ symb "deep_quote"

symbDeepUnquote :: Symb
symbDeepUnquote = quoteSymb $ symb "deep_unquote"