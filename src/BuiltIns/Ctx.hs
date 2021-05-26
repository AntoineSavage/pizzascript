module BuiltIns.Ctx where

import qualified Data.Map as M

import BuiltIns.Values ( pzFalse, pzTrue, pzNot, pzOr, pzAnd, pzFunc )
import Types.PzVal ( Dict, PzVal(PzSymb) )
import Symbs ( symbAnd, symbFunc, symbNot, symbOr )

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
    , (PzSymb symbNot, pzNot)
    , (PzSymb symbOr, pzOr)
    , (PzSymb symbAnd, pzAnd)

    -- lists
    -- TODO

    -- dictionaries
    -- TODO

    -- functions
    , (PzSymb symbFunc, pzFunc)

    -- miscellaneous
    -- TODO
    ]