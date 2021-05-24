module BuiltIns.Ctx where

import qualified Data.Map as M

import BuiltIns.Values ( pzFalse, pzTrue, pzNot, pzOr, pzAnd, pzFunc )
import Data.PzVal ( Dict, PzVal(PzSymb) )
import Data.Symb ( symb )
import Idents ( identAnd, identFunc, identNot, identOr )

builtInCtx :: Dict
builtInCtx = M.fromList
    [
    -- numbers
    -- TODO

    -- strings
    -- TODO

    -- symbols
    -- TODO

    -- booleans
      (pzFalse, pzFalse)
    , (pzTrue, pzTrue)
    , (PzSymb $ symb identNot, pzNot)
    , (PzSymb $ symb identOr, pzOr)
    , (PzSymb $ symb identAnd, pzAnd)

    -- lists
    -- TODO

    -- dictionaries
    -- TODO

    -- functions
    , (PzSymb $ symb identFunc, pzFunc)

    -- miscellaneous
    -- TODO
    ]