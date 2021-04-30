module BuiltIns where

import qualified Data.Map as M

import Types
import Utils

{- TODO for base:
    - func
    - is_empty, size (list / dict)
    - cons, head, tail
    - keys, get, put, del
-}
-- Built-in context
builtInCtx :: Dict
builtInCtx = M.fromList
    [ (pzFalse, pzFalse)
    , (pzTrue, pzTrue)
    , (PzSymb $ symb $ ident "not", pzNot)
    , (PzSymb $ symb $ ident "or", pzOr)
    , (PzSymb $ symb $ ident "and", pzAnd)
    ]

-- values
pzFalse :: PzVal
pzFalse = PzSymb $ symb identFalse

pzTrue :: PzVal
pzTrue = PzSymb $ symb identTrue

pzNot :: PzVal
pzNot = PzFunc $ Func Eval Nothing (ArgsArity [ident "x"]) $ BodyBuiltIn "_not"

pzOr :: PzVal
pzOr = PzFunc $ Func Eval Nothing (ArgsArity [ident "x", ident "y"]) $ BodyBuiltIn "_or"

pzAnd :: PzVal
pzAnd = PzFunc $ Func Eval Nothing (ArgsArity [ident "x", ident "y"]) $ BodyBuiltIn "_and"

-- built-in implementations
_not :: Dict -> [PzVal] -> FuncReturn
_not ctx args = f1 args $ \x -> return $ (,) ctx $ case boolish x of
    FalseReal -> pzTrue
    Falsish -> pzTrue
    Truish -> pzFalse
    TrueReal -> pzFalse

_or :: Dict -> [PzVal] -> FuncReturn
_or ctx args = f2 args $ \x y -> return $ (,) ctx $
    case (boolish x, boolish y) of
        (TrueReal , _)            -> x
        (Truish   , TrueReal)     -> y
        (Truish   , _)            -> x
        (Falsish  , FalseReal)    -> x
        (Falsish  , _)            -> y
        (FalseReal, _)            -> y

_and :: Dict -> [PzVal] -> FuncReturn
_and ctx args = f2 args $ \x y -> return $ (,) ctx $
    case (boolish x, boolish y) of
        (FalseReal, _)            -> x
        (Falsish  , FalseReal)    -> y
        (Falsish  , _)            -> x
        (Truish   , TrueReal)     -> x
        (Truish   , _)            -> y
        (TrueReal , _)            -> y

-- Utils
data Boolish
    = FalseReal
    | Falsish
    | Truish
    | TrueReal
    deriving (Show, Eq)

boolish :: PzVal -> Boolish
boolish v
  | v == pzFalse = FalseReal
  | v == pzTrue = TrueReal
  | otherwise = case v of
    PzUnit -> Falsish
    PzNum 0 -> Falsish
    PzStr "" -> Falsish
    PzList [] -> Falsish
    PzDict d -> if M.null d then Falsish else Truish
    _ -> Truish