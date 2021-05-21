module BuiltIns.Impls where

import BuiltIns.Values ( pzFalse, pzTrue )
import Data.PzVal ( Dict, PzVal )
import Data.WithPos ( WithPos )
import Data.Boolish ( Boolish(..), boolish )

type FuncReturn = Either String (Dict, WithPos PzVal)

-- numbers
-- TODO

-- strings
-- TODO

-- symbols
-- TODO

-- booleans
_not :: WithPos PzVal -> WithPos PzVal
_not x = case boolish x of
    FalseReal -> pzTrue
    Falsish -> pzTrue
    Truish -> pzFalse
    TrueReal -> pzFalse

_or :: WithPos PzVal -> WithPos PzVal -> WithPos PzVal
_or x y = case (boolish x, boolish y) of
    (TrueReal , _)            -> x
    (Truish   , TrueReal)     -> y
    (Truish   , _)            -> x
    (Falsish  , FalseReal)    -> x
    (Falsish  , _)            -> y
    (FalseReal, _)            -> y

_and :: WithPos PzVal -> WithPos PzVal -> WithPos PzVal
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