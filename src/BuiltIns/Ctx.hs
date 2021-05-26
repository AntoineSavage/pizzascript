module BuiltIns.Ctx where

import qualified Data.Map as M

import BuiltIns.Values ( pzNot, pzOr, pzAnd, pzFunc )
import Types.PzVal ( Dict, PzVal(..) )
import Symbs ( symbFunc, pzSymbFalse, pzSymbTrue, pzSymbNot, pzSymbOr, pzSymbAnd )

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
      (pzSymbFalse, pzSymbFalse)
    , (pzSymbTrue, pzSymbTrue)
    , (pzSymbNot, pzNot)
    , (pzSymbOr, pzOr)
    , (pzSymbAnd, pzAnd)

    -- lists
    -- TODO

    -- dictionaries
    -- TODO

    -- functions
    , (PzSymb symbFunc, pzFunc)

    -- miscellaneous
    -- TODO
    ]