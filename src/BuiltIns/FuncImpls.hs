{-# LANGUAGE LambdaCase #-}
module BuiltIns.FuncImpls where

import qualified Data.Map as M

import Eval ( evalFuncCustom )
import Ops.Boolish ( boolish )
import Ops.Numb
import Ops.Func.FuncCustom ( fromFuncCustom )
import Symbs
import Text.Parsec ( parse )
import Types.Boolish ( Boolish(..) )
import Types.Numb ( Numb(..) )
import Types.PzVal ( Dict, DictKey(..), PzVal(..) )
import Types.Str ( Str(..) )
import Utils ( Result )

-- generic
_typeOf :: PzVal -> PzVal
_typeOf = \case
    PzUnit -> PzUnit
    PzNum _ -> pzSymbNum
    PzStr _ -> pzSymbStr
    PzSymb _ -> pzSymbSymb
    PzList _ -> pzSymbList
    PzDict _ -> pzSymbDict
    PzFunc _ _ -> pzSymbFunc

_eq :: PzVal -> PzVal -> PzVal
_eq x y = toBool $ DictKey x == DictKey y

_lt :: PzVal -> PzVal -> PzVal
_lt x y = toBool $ DictKey x < DictKey y

-- semi-generic
_isEmpty :: PzVal -> Result PzVal
_isEmpty v = toBool <$> case v of
    PzStr (Str s) -> return $ null s
    PzList l      -> return $ null l
    PzDict d      -> return $ M.null d
    _             -> Left $
        "Function 'is_empty only supports strings, lists and dictionaries"
            ++ "\n was: " ++ show v

_size :: PzVal -> Result PzVal
_size v = toInt <$> case v of
    PzStr (Str s) -> return $ length s
    PzList l      -> return $ length l
    PzDict d      -> return $ M.size d
    _             -> Left $
        "Function 'size only supports strings, lists and dictionaries"
            ++ "\n was: " ++ show v

-- numbers
_num :: PzVal -> Result PzVal
_num v = case v of
    PzNum _ -> return v
    PzStr (Str s) -> case parse parseNumb "Call to function 'num" s of
        Right n -> return $ PzNum n
        Left err -> Left $ show err
    _             -> Left $
        "Function 'num only supports numbers and strings"
            ++ "\n was: " ++ show v

_add :: PzVal -> PzVal -> Result PzVal
_add = undefined

_sub :: PzVal -> PzVal -> Result PzVal
_sub = undefined

_mult :: PzVal -> PzVal -> Result PzVal
_mult = undefined

_div :: PzVal -> PzVal -> Result PzVal
_div = undefined

_rem :: PzVal -> PzVal -> Result PzVal
_rem = undefined

_exp :: PzVal -> PzVal -> Result PzVal
_exp = undefined

_log :: PzVal -> PzVal -> Result PzVal
_log = undefined

_round :: PzVal -> Result PzVal
_round = undefined

_floor :: PzVal -> Result PzVal
_floor = undefined

_ceil :: PzVal -> Result PzVal
_ceil = undefined

_trunc :: PzVal -> Result PzVal
_trunc = undefined

-- strings
_str :: [PzVal] -> PzVal
_str = undefined

_split :: PzVal -> PzVal -> Result PzVal
_split = undefined

_join :: [PzVal] -> Result PzVal
_join = undefined

-- symbols
_symb :: PzVal -> Result PzVal
_symb = undefined

_nbrQuotes :: PzVal -> Result PzVal
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
_cons :: PzVal -> PzVal -> Result PzVal
_cons = undefined

_head :: PzVal -> Result PzVal
_head = undefined

_tail :: PzVal -> Result PzVal
_tail = undefined

-- dictionaries
_keys :: PzVal -> Result PzVal
_keys = undefined

_assocs :: PzVal -> Result PzVal
_assocs = undefined

_contains :: PzVal -> PzVal -> Result PzVal
_contains = undefined

_get :: PzVal -> PzVal -> Result PzVal
_get = undefined

_put :: PzVal -> PzVal -> PzVal -> Result PzVal
_put = undefined

_del :: PzVal -> PzVal -> Result PzVal
_del = undefined

-- functions
_func :: Dict -> [PzVal] -> Result PzVal
_func ctx elems = do
    fc <- evalFuncCustom elems
    let f = fromFuncCustom fc
    return $ PzList [PzDict ctx, PzFunc ctx f]

_getImplCtx :: PzVal -> Result PzVal
_getImplCtx = undefined

_setImplCtx :: PzVal -> PzVal -> Result PzVal
_setImplCtx = undefined

_getExplCtx :: PzVal -> Result PzVal
_getExplCtx = undefined

_getArgPass :: PzVal -> Result PzVal
_getArgPass = undefined

_getArgs :: PzVal -> Result PzVal
_getArgs = undefined

_getBody :: PzVal -> Result PzVal
_getBody = undefined

-- Utils
toBool :: Bool -> PzVal
toBool p = if p then pzSymbTrue else pzSymbFalse

toInt :: Int -> PzVal
toInt = PzNum . Numb . fromIntegral