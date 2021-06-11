{-# LANGUAGE LambdaCase #-}
module BuiltIns.FuncImpls where

import qualified Data.Map as M

import Control.Monad ( liftM2 )
import Data.List ( intercalate, stripPrefix )
import Data.Maybe ( fromMaybe, isJust )
import Eval ( evalFuncCustom, uneval )
import Ops.Boolish ( boolish )
import Ops.Numb ( parseNumb )
import Ops.Func.ArgPass ( argPassToSymb )
import Ops.Func.FuncCustom ( fromFuncCustom )
import Ops.Func.FuncImpureArgs ( getArgPass, getExplCtx )
import Ops.PzVal ( fromQuoted, unDictKey )
import Ops.Symb ( getNbrQuotes, parseSymb )
import Symbs ( pzSymbFalse, pzSymbTrue, pzSymbFunc, pzSymbDict, pzSymbList, pzSymbSymb, pzSymbStr, pzSymbNum )
import Text.Parsec ( parse )
import Types.Boolish ( Boolish(..) )
import Types.Func ( Func(..) )
import Types.Func.FuncArgs ( FuncArgs(..) )
import Types.Func.FuncBody ( FuncBody(..) )
import Types.Func.FuncImpureArgs ( FuncImpureArgs(..) )
import Types.Numb ( Numb(..) )
import Types.PzVal ( Dict, DictKey(..), Evaled, PzVal(..), Quoted )
import Types.Str ( Str(..) )
import Utils ( Result, unparse )

-- generic
_typeOf :: PzVal Evaled -> PzVal Evaled
_typeOf = \case
    PzUnit -> PzUnit
    PzNum _ -> pzSymbNum
    PzStr _ -> pzSymbStr
    PzSymb _ -> pzSymbSymb
    PzList _ -> pzSymbList
    PzDict _ -> pzSymbDict
    PzFunc _ _ -> pzSymbFunc

_eq :: PzVal Evaled -> PzVal Evaled -> PzVal Evaled
_eq x y = toBool $ DictKey x == DictKey y

_lt :: PzVal Evaled -> PzVal Evaled -> PzVal Evaled
_lt x y = toBool $ DictKey x < DictKey y

-- semi-generic
_isEmpty :: PzVal Evaled -> Result (PzVal Evaled)
_isEmpty v = toBool <$> case v of
    PzStr (Str s) -> return $ null s
    PzList l      -> return $ null l
    PzDict d      -> return $ M.null d
    _             -> Left $
        "Function 'is_empty only supports strings, lists and dictionaries"
            ++ "\n was: " ++ show v

_size :: PzVal Evaled -> Result (PzVal Evaled)
_size v = toInt <$> case v of
    PzStr (Str s) -> return $ length s
    PzList l      -> return $ length l
    PzDict d      -> return $ M.size d
    _             -> Left $
        "Function 'size only supports strings, lists and dictionaries"
            ++ "\n was: " ++ show v

-- numbers
_num :: PzVal Evaled -> Result (PzVal Evaled)
_num v = case v of
    PzNum _ -> return v
    PzStr (Str s) -> case parse parseNumb "Call to function 'num" s of
        Right n -> return $ PzNum n
        Left err -> Left $ show err
    _             -> Left $
        "Function 'num only supports numbers and strings"
            ++ "\n was: " ++ show v

_add :: PzVal Evaled -> PzVal Evaled -> Result (PzVal Evaled)
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

_sub :: PzVal Evaled -> PzVal Evaled -> Result (PzVal Evaled)
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

_mult :: PzVal Evaled -> PzVal Evaled -> Result (PzVal Evaled)
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

_div :: PzVal Evaled -> PzVal Evaled -> Result (PzVal Evaled)
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

_rem :: PzVal Evaled -> PzVal Evaled -> Result (PzVal Evaled)
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

_exp :: PzVal Evaled -> PzVal Evaled -> Result (PzVal Evaled)
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

_log :: PzVal Evaled -> PzVal Evaled -> Result (PzVal Evaled)
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

_round :: PzVal Evaled -> Result (PzVal Evaled)
_round v = case v of
    PzNum (Numb x) -> return $ PzNum $ Numb $ fromIntegral $ round x
    _              -> Left $
        "Function 'round only supports numbers"
            ++ "\n was: " ++ show v

_floor :: PzVal Evaled -> Result (PzVal Evaled)
_floor v = case v of
    PzNum (Numb x) -> return $ PzNum $ Numb $ fromIntegral $ floor x
    _              -> Left $
        "Function 'floor only supports numbers"
            ++ "\n was: " ++ show v

_ceil :: PzVal Evaled -> Result (PzVal Evaled)
_ceil v = case v of
    PzNum (Numb x) -> return $ PzNum $ Numb $ fromIntegral $ ceiling x
    _              -> Left $
        "Function 'ceil only supports numbers"
            ++ "\n was: " ++ show v

_trunc :: PzVal Evaled -> Result (PzVal Evaled)
_trunc v = case v of
    PzNum (Numb x) -> return $ PzNum $ Numb $ fromIntegral $ truncate x
    _              -> Left $
        "Function 'trunc only supports numbers"
            ++ "\n was: " ++ show v

-- strings
_str :: [PzVal Evaled] -> PzVal Evaled
_str = PzStr . Str . concatMap toStr where
    toStr (PzStr (Str s)) = s
    toStr v = unparse $ uneval v

_split :: PzVal Evaled -> PzVal Evaled -> Result (PzVal Evaled)
_split a b = case (a, b) of
    (PzStr (Str x), PzStr (Str y)) -> return $ PzList $ map (PzStr . Str) $ actualSplit x y
    _ -> Left $
        "Function 'split only supports strings"
            ++ "\n was: " ++ show a
            ++ "\n and: " ++ show b

_join :: [PzVal Evaled] -> Result (PzVal Evaled)
_join =
    let flatten = \case
            [] -> return []
            x:xs -> liftM2 (flip (++)) (flatten xs) $ case x of
                PzStr (Str s) -> return [s]
                PzList ys -> flatten ys
                x -> Left $ "Function 'join only support strings or lists of strings (second or more arg)"
                        ++ "\n was: " ++ show x

    in \case
        [] -> Left "Function 'join requires at least two arguments, but got zero"
        [x] -> Left $ "Function 'join requires at least two arguments, but got one"
            ++ "\n was: " ++ show x

        x:xs -> do
            sep <- case x of
                PzStr (Str s) -> return s
                v -> Left $ "Function 'join only support strings (first arg)"
                    ++ "\n was: " ++ show v

            ss <- flatten xs
            return $ PzStr $ Str $ actualJoin sep ss

-- symbols
_symb :: PzVal Evaled -> Result (PzVal Evaled)
_symb v = case v of
    PzStr (Str s) -> case parse parseSymb "Call to function 'symb" s of
        Right sym -> return $ PzSymb sym
        Left err -> Left $ show err
    PzSymb s -> return v
    _ -> Left $
        "Function 'symb only supports strings and symbols"
            ++ "\n was: " ++ show v

_nbrQuotes :: PzVal Evaled -> Result (PzVal Evaled)
_nbrQuotes = \case
    PzSymb s -> return $ PzNum $ Numb $ fromIntegral $ getNbrQuotes s
    v -> Left $
        "Function 'nbr_quotes only supports symbols"
            ++ "\n was: " ++ show v

-- booleans
_not :: PzVal Evaled -> PzVal Evaled
_not x = case boolish x of
    FalseReal -> pzSymbTrue
    Falsish -> pzSymbTrue
    Truish -> pzSymbFalse
    TrueReal -> pzSymbFalse

_or :: PzVal Evaled -> PzVal Evaled -> PzVal Evaled
_or x y = case (boolish x, boolish y) of
    (TrueReal , _)            -> x
    (Truish   , TrueReal)     -> y
    (Truish   , _)            -> x
    (Falsish  , FalseReal)    -> x
    (Falsish  , _)            -> y
    (FalseReal, _)            -> y

_and :: PzVal Evaled -> PzVal Evaled -> PzVal Evaled
_and x y = case (boolish x, boolish y) of
    (FalseReal, _)            -> x
    (Falsish  , FalseReal)    -> y
    (Falsish  , _)            -> x
    (Truish   , TrueReal)     -> x
    (Truish   , _)            -> y
    (TrueReal , _)            -> y

-- lists
_cons :: PzVal Evaled -> PzVal Evaled -> Result (PzVal Evaled)
_cons x = \case
    PzList xs -> return $ PzList $ x:xs
    v -> Left $
        "Function 'cons only supports lists (second arg)"
            ++ "\n was: " ++ show v

_head :: PzVal Evaled -> Result (PzVal Evaled)
_head = \case
    PzList (x:_) -> return x
    v -> Left $
        "Function 'head only supports non-empty lists"
            ++ "\n was: " ++ show v

_tail :: PzVal Evaled -> Result (PzVal Evaled)
_tail = \case
    PzList (_:xs) -> return $ PzList xs
    v -> Left $
        "Function 'tail only supports non-empty lists"
            ++ "\n was: " ++ show v

-- dictionaries
_keys :: PzVal Evaled -> Result (PzVal Evaled)
_keys = \case
    PzDict d -> return $ PzList $ map unDictKey $ M.keys d
    v -> Left $
        "Function 'keys only supports dictionaries"
            ++ "\n was: " ++ show v

_assocs :: PzVal Evaled -> Result (PzVal Evaled)
_assocs = \case
    PzDict d -> return $ PzList $ flip map (M.assocs d) $ \(DictKey k, v) -> PzList [k, v]
    v -> Left $
        "Function 'assocs only supports dictionaries"
            ++ "\n was: " ++ show v

_contains :: PzVal Evaled -> PzVal Evaled -> Result (PzVal Evaled)
_contains x k = case x of
    PzDict d -> return $ toBool $ isJust $ M.lookup (DictKey k) d
    v -> Left $
        "Function 'contains only supports dictionaries (first arg)"
            ++ "\n was: " ++ show v

_get :: PzVal Evaled -> PzVal Evaled -> Result (PzVal Evaled)
_get x k = case x of
    PzDict d -> return $ fromMaybe PzUnit $ M.lookup (DictKey k) d
    v -> Left $
        "Function 'get only supports dictionaries (first arg)"
            ++ "\n was: " ++ show v

_put :: PzVal Evaled -> PzVal Evaled -> PzVal Evaled -> Result (PzVal Evaled)
_put x k v = case x of
    PzDict d -> return $ PzDict $ M.insert (DictKey k) v d
    v -> Left $
        "Function 'put only supports dictionaries (first arg)"
            ++ "\n was: " ++ show v

_del :: PzVal Evaled -> PzVal Evaled -> Result (PzVal Evaled)
_del x k = case x of
    PzDict d -> return $ PzDict $ M.delete (DictKey k) d
    v -> Left $
        "Function 'del only supports dictionaries (first arg)"
            ++ "\n was: " ++ show v

-- functions
_func :: Dict -> [PzVal Quoted] -> Result (PzVal Evaled)
_func ctx elems = do
    fc <- evalFuncCustom elems
    let f = fromFuncCustom fc
    return $ PzList [PzDict ctx, PzFunc ctx f]

_getImplCtx :: PzVal Evaled -> Result (PzVal Evaled)
_getImplCtx = \case
    PzFunc d _ -> return $ PzDict d
    v -> Left $
        "Function 'get_impl_ctx only supports functions"
            ++ "\n was: " ++ show v

_setImplCtx :: PzVal Evaled -> PzVal Evaled -> Result (PzVal Evaled)
_setImplCtx x y = case (x, y) of
    (PzFunc _ f, PzDict d) -> return $ PzFunc d f
    v -> Left $
        "Function 'set_impl_ctx only supports functions (first arg) and dictionaries (second arg)"
            ++ "\n was: " ++ show x
            ++ "\n and: " ++ show y

_getExplCtx :: PzVal Evaled -> Result (PzVal Evaled)
_getExplCtx = \case
    PzFunc _ (Func ia _ _) -> return $ case getExplCtx ia of
        Just ec -> PzSymb ec
        _       -> PzUnit
    v -> Left $
        "Function 'get_expl_ctx only supports functions"
            ++ "\n was: " ++ show v

_getArgPass :: PzVal Evaled -> Result (PzVal Evaled)
_getArgPass = \case
    PzFunc _ (Func ia _ _) -> return $ PzSymb $ argPassToSymb $ getArgPass ia
    v -> Left $
        "Function 'get_arg_pass only supports functions"
            ++ "\n was: " ++ show v

_getArgs :: PzVal Evaled -> Result (PzVal Evaled)
_getArgs = \case
    PzFunc _ (Func _ a _) -> case a of
        ArgsVaria s -> return $ PzSymb s
        ArgsArity ss -> return $ PzList $ map PzSymb ss
    v -> Left $
        "Function 'get_args only supports functions"
            ++ "\n was: " ++ show v

_getBody :: PzVal Evaled -> Result (PzVal Evaled)
_getBody = \case
    PzFunc _ (Func _ _ b) -> case b of
        BodyBuiltIn s -> return $ PzSymb s
        BodyCustom x xs -> return $ PzList $ map fromQuoted $ x:xs
    v -> Left $
        "Function 'get_body only supports functions"
            ++ "\n was: " ++ show v

-- Utils
toBool :: Bool -> PzVal Evaled
toBool p = if p then pzSymbTrue else pzSymbFalse

toInt :: Int -> PzVal Evaled
toInt = PzNum . Numb . fromIntegral

isValidNum :: Double -> Bool
isValidNum d = not $ isNaN d || isInfinite d

actualSplit :: String -> String -> [String]
actualSplit "" = map (:[])
actualSplit sep = go [] "" where
    addPrefix acc prefix = (++acc) $ case prefix of
        "" -> []
        cs -> [reverse cs]

    go acc prefix "" = reverse $ addPrefix acc prefix
    go acc prefix suffix@(c:cs) = case stripPrefix sep suffix of
        Nothing -> go acc (c:prefix) cs
        Just s -> go (addPrefix acc prefix) "" s
    
actualJoin :: String -> [String] -> String
actualJoin = intercalate