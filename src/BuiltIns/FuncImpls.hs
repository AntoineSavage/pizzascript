{-# LANGUAGE LambdaCase #-}
module BuiltIns.FuncImpls where

import qualified Data.Map as M

import Eval ( evalFuncCustom )
import Ops.Boolish ( boolish )
import Ops.Numb ( parseNumb )
import Ops.Func.ArgPass ( argPassToSymb )
import Ops.Func.FuncCustom ( fromFuncCustom )
import Ops.Func.FuncImpureArgs ( getArgPass, getExplCtx )
import Symbs
import Text.Parsec ( parse )
import Types.Boolish ( Boolish(..) )
import Types.Func ( Func(..) )
import Types.Func.FuncArgs ( FuncArgs(..) )
import Types.Func.FuncBody ( FuncBody(..) )
import Types.Func.FuncImpureArgs ( FuncImpureArgs(..) )
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
_add a b = case (a, b) of
    (PzNum (Numb x), PzNum (Numb y)) ->
        let result = x + y
        in if isValidNum result
            then return $ PzNum $ Numb result
            else Left $ "Invalid addition: " ++ show x ++ " + " ++ show y
    _             -> Left $
        "Function 'add only supports numbers"
            ++ "\n was: " ++ show a
            ++ "\n and: " ++ show b

_sub :: PzVal -> PzVal -> Result PzVal
_sub a b = case (a, b) of
    (PzNum (Numb x), PzNum (Numb y)) ->
        let result = x - y
        in if isValidNum result
            then return $ PzNum $ Numb result
            else Left $ "Invalid substraction: " ++ show x ++ " - " ++ show y
    _             -> Left $
        "Function 'sub only supports numbers"
            ++ "\n was: " ++ show a
            ++ "\n and: " ++ show b

_mult :: PzVal -> PzVal -> Result PzVal
_mult a b = case (a, b) of
    (PzNum (Numb x), PzNum (Numb y)) ->
        let result = x * y
        in if isValidNum result
            then return $ PzNum $ Numb result
            else Left $ "Invalid multiplication: " ++ show x ++ " * " ++ show y
    _             -> Left $
        "Function 'mult only supports numbers"
            ++ "\n was: " ++ show a
            ++ "\n and: " ++ show b

_div :: PzVal -> PzVal -> Result PzVal
_div a b = case (a, b) of
    (PzNum (Numb x), PzNum (Numb y)) ->
        let result = x / y
        in if y /= 0 && isValidNum result
            then return $ PzNum $ Numb result
            else Left $ "Invalid division: " ++ show x ++ " / " ++ show y
    _             -> Left $
        "Function 'div only supports numbers"
            ++ "\n was: " ++ show a
            ++ "\n and: " ++ show b

_rem :: PzVal -> PzVal -> Result PzVal
_rem a b = case (a, b) of
    (PzNum (Numb x), PzNum (Numb y)) ->
        let result = fromIntegral $ truncate x `rem` truncate y
        in if  y /= 0 && isValidNum result
            then return $ PzNum $ Numb result
            else Left $ "Invalid remainder: " ++ show x ++ " % " ++ show y
    _             -> Left $
        "Function 'rem only supports numbers"
            ++ "\n was: " ++ show a
            ++ "\n and: " ++ show b

_exp :: PzVal -> PzVal -> Result PzVal
_exp a b = case (a, b) of
    (PzNum (Numb x), PzNum (Numb y)) ->
        let result = x ** y
        in if isValidNum result
            then return $ PzNum $ Numb result
            else Left $ "Invalid exponentiation: " ++ show x ++ " ^ " ++ show y
    _             -> Left $
        "Function 'exp only supports numbers"
            ++ "\n was: " ++ show a
            ++ "\n and: " ++ show b

_log :: PzVal -> PzVal -> Result PzVal
_log a b = case (a, b) of
    (PzNum (Numb x), PzNum (Numb y)) ->
        let result = logBase x y
        in if isValidNum result
            then return $ PzNum $ Numb result
            else Left $ "Invalid logarithm: log (base " ++ show x ++ ") " ++ show y
    _             -> Left $
        "Function 'log only supports numbers"
            ++ "\n was: " ++ show a
            ++ "\n and: " ++ show b

_round :: PzVal -> Result PzVal
_round v = case v of
    PzNum (Numb x) -> return $ PzNum $ Numb $ fromIntegral $ round x
    _              -> Left $
        "Function 'round only supports numbers"
            ++ "\n was: " ++ show v

_floor :: PzVal -> Result PzVal
_floor v = case v of
    PzNum (Numb x) -> return $ PzNum $ Numb $ fromIntegral $ floor x
    _              -> Left $
        "Function 'floor only supports numbers"
            ++ "\n was: " ++ show v

_ceil :: PzVal -> Result PzVal
_ceil v = case v of
    PzNum (Numb x) -> return $ PzNum $ Numb $ fromIntegral $ ceiling x
    _              -> Left $
        "Function 'ceil only supports numbers"
            ++ "\n was: " ++ show v

_trunc :: PzVal -> Result PzVal
_trunc v = case v of
    PzNum (Numb x) -> return $ PzNum $ Numb $ fromIntegral $ truncate x
    _              -> Left $
        "Function 'trunc only supports numbers"
            ++ "\n was: " ++ show v

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
_getImplCtx = \case
    PzFunc d _ -> return $ PzDict d
    v -> Left $
        "Function 'get_impl_ctx only supports functions"
            ++ "\n was: " ++ show v

_setImplCtx :: PzVal -> PzVal -> Result PzVal
_setImplCtx x y = case (x, y) of
    (PzFunc _ f, PzDict d) -> return $ PzFunc d f
    v -> Left $
        "Function 'set_impl_ctx only supports functions (first arg) and dictionaries (second arg)"
            ++ "\n was: " ++ show x
            ++ "\n and: " ++ show y

_getExplCtx :: PzVal -> Result PzVal
_getExplCtx = \case
    PzFunc _ (Func ia _ _) -> return $ case getExplCtx ia of
        Just ec -> PzSymb ec
        _       -> PzUnit
    v -> Left $
        "Function 'get_expl_ctx only supports functions"
            ++ "\n was: " ++ show v

_getArgPass :: PzVal -> Result PzVal
_getArgPass = \case
    PzFunc _ (Func ia _ _) -> return $ PzSymb $ argPassToSymb $ getArgPass ia
    v -> Left $
        "Function 'get_arg_pass only supports functions"
            ++ "\n was: " ++ show v

_getArgs :: PzVal -> Result PzVal
_getArgs = \case
    PzFunc _ (Func _ a _) -> case a of
        ArgsVaria s -> return $ PzSymb s
        ArgsArity ss -> return $ PzList $ map PzSymb ss
    v -> Left $
        "Function 'get_args only supports functions"
            ++ "\n was: " ++ show v

_getBody :: PzVal -> Result PzVal
_getBody = \case
    PzFunc _ (Func _ _ b) -> case b of
        BodyBuiltIn s -> return $ PzSymb s
        BodyCustom x xs -> return $ PzList $ x:xs
    v -> Left $
        "Function 'get_body only supports functions"
            ++ "\n was: " ++ show v

-- Utils
toBool :: Bool -> PzVal
toBool p = if p then pzSymbTrue else pzSymbFalse

toInt :: Int -> PzVal
toInt = PzNum . Numb . fromIntegral

isValidNum :: Double -> Bool
isValidNum d = not $ isNaN d || isInfinite d