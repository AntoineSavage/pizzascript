module BuiltIns.FuncImpls where

import Ops.Boolish ( boolish )
import Symbs ( pzSymbFalse, pzSymbTrue )
import Types.PzVal ( PzVal )
import Types.Boolish ( Boolish(..) )

-- generic
_typeOf :: PzVal -> PzVal
_typeOf = undefined

_eq :: PzVal -> PzVal -> PzVal
_eq = undefined

_lt :: PzVal -> PzVal -> PzVal
_lt = undefined

-- semi-generic
_isEmpty :: PzVal -> PzVal
_isEmpty = undefined

_size :: PzVal -> PzVal
_size = undefined

-- numbers
_num :: PzVal -> PzVal
_num = undefined

_add :: PzVal -> PzVal -> PzVal
_add = undefined

_sub :: PzVal -> PzVal -> PzVal
_sub = undefined

_mult :: PzVal -> PzVal -> PzVal
_mult = undefined

_div :: PzVal -> PzVal -> PzVal
_div = undefined

_rem :: PzVal -> PzVal -> PzVal
_rem = undefined

_exp :: PzVal -> PzVal -> PzVal
_exp = undefined

_log :: PzVal -> PzVal -> PzVal
_log = undefined

_round :: PzVal -> PzVal
_round = undefined

_floor :: PzVal -> PzVal
_floor = undefined

_ceil :: PzVal -> PzVal
_ceil = undefined

_trunc :: PzVal -> PzVal
_trunc = undefined

-- strings
_str :: [PzVal] -> PzVal
_str = undefined

_split :: PzVal -> PzVal -> PzVal
_split = undefined

_join :: [PzVal] -> PzVal
_join = undefined

-- symbols
_symb :: PzVal -> PzVal
_symb = undefined

_nbrQuotes :: PzVal -> PzVal
_nbrQuotes = undefined

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
_cons :: PzVal -> PzVal -> PzVal
_cons = undefined

_head :: PzVal -> PzVal
_head = undefined

_tail :: PzVal -> PzVal
_tail = undefined

-- dictionaries
_keys :: PzVal -> PzVal
_keys = undefined

_assocs :: PzVal -> PzVal
_assocs = undefined

_contains :: PzVal -> PzVal -> PzVal
_contains = undefined

_get :: PzVal -> PzVal -> PzVal
_get = undefined

_put :: PzVal -> PzVal -> PzVal -> PzVal
_put = undefined

_del :: PzVal -> PzVal -> PzVal
_del = undefined

-- functions
_getImplCtx :: PzVal -> PzVal
_getImplCtx = undefined

_setImplCtx :: PzVal -> PzVal -> PzVal
_setImplCtx = undefined

_getExplCtx :: PzVal -> PzVal
_getExplCtx = undefined

_getArgPass :: PzVal -> PzVal
_getArgPass = undefined

_getArgs :: PzVal -> PzVal
_getArgs = undefined

_getBody :: PzVal -> PzVal
_getBody = undefined