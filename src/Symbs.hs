module Symbs where

import Data.Nat ( Nat(Z, S) ) 
import Data.Symb ( Symb(..), symb )

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
symbEval = Symb (S Z) 'e' "val"

symbQuote :: Symb
symbQuote = Symb (S Z) 'q' "uote"

symbUnquote :: Symb
symbUnquote = Symb (S Z) 'u' "nquote"

symbDeepQuote :: Symb
symbDeepQuote = Symb (S Z) 'd' "eep_quote"

symbDeepUnquote :: Symb
symbDeepUnquote = Symb (S Z) 'd' "eep_unquote"