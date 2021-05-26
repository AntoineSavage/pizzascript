module BuiltIns.Impls where

import Ops.Boolish ( boolish )
import Symbs ( pzSymbFalse, pzSymbTrue )
import Types.PzVal ( PzVal )
import Types.Boolish ( Boolish(..) )

-- numbers
-- TODO

-- strings
-- TODO

-- symbols
-- TODO

-- booleans
_not :: PzVal -> PzVal
_not x = case boolish x of
    FalseReal -> pzSymbTrue
    Falsish -> pzSymbTrue
    Truish -> pzSymbFalse
    TrueReal -> pzSymbFalse

_or :: PzVal -> PzVal -> PzVal
_or x y = case (boolish x, boolish y) of
    (TrueReal , _)            -> x
    (Truish   , TrueReal)     -> y
    (Truish   , _)            -> x
    (Falsish  , FalseReal)    -> x
    (Falsish  , _)            -> y
    (FalseReal, _)            -> y

_and :: PzVal -> PzVal -> PzVal
_and x y = case (boolish x, boolish y) of
    (FalseReal, _)            -> x
    (Falsish  , FalseReal)    -> y
    (Falsish  , _)            -> x
    (Truish   , TrueReal)     -> x
    (Truish   , _)            -> y
    (TrueReal , _)            -> y

-- lists
-- TODO

-- dictionaries
-- TODO

-- functions
-- TODO

-- miscellaneous
-- TODO