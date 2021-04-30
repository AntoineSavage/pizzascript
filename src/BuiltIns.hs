module BuiltIns where

import qualified Data.Map as M

import Types
import Utils

-- constants
false :: PzVal
false = PzSymb $ symb identFalse

true :: PzVal
true = PzSymb $ symb identTrue

-- functions
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