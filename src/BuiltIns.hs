{-# LANGUAGE TupleSections #-}
module BuiltIns where

import qualified Data.Map as M

import Text.Parsec.Pos ( newPos )
import Types
import Utils

-- Built-in context
builtInPos :: Pos
builtInPos = newPos "<built-in>" 0 0

withPos :: a -> WithPos a
withPos = WithPos builtInPos

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
    , (withPos $ PzSymb $ symb identNot, pzNot)
    , (withPos $ PzSymb $ symb identOr, pzOr)
    , (withPos $ PzSymb $ symb identAnd, pzAnd)

    -- lists
    -- TODO

    -- dictionaries
    -- TODO

    -- functions
    , (withPos $ PzSymb $ symb identFunc, pzFunc)

    -- miscellaneous
    -- TODO
    ]

------------
-- Values
------------

-- numbers
-- TODO

-- strings
-- TODO

-- symbols
-- TODO

-- booleans
pzFalse :: WithPos PzVal
pzFalse = withPos $ PzSymb symbFalse

pzTrue :: WithPos PzVal
pzTrue = withPos $ PzSymb symbTrue

pzNot :: WithPos PzVal
pzNot = withPos $ PzFunc $ Func M.empty Nothing Eval (ArgsArity [identX]) $ BodyBuiltIn identNot

pzOr :: WithPos PzVal
pzOr = withPos $ PzFunc $ Func M.empty Nothing Eval (ArgsArity [identX, identY]) $ BodyBuiltIn identOr

pzAnd :: WithPos PzVal
pzAnd = withPos $ PzFunc $ Func M.empty Nothing Eval (ArgsArity [identX, identY]) $ BodyBuiltIn identAnd

-- lists
-- TODO

-- dictionaries
-- TODO

-- functions
pzFunc :: WithPos PzVal
pzFunc = withPos $ PzFunc $ Func M.empty (Just identCtx) Quote (ArgsVaria identArgs) $ BodyBuiltIn identFunc

-- miscellaneous
-- TODO

-------------
-- Implems
-------------

type FuncReturn = Either String (Dict, WithPos PzVal)

-- numbers
-- TODO

-- strings
-- TODO

-- symbols
symbSplitImpl :: Symb -> [Symb]
symbSplitImpl (Symb n (Ident ps)) = map (Symb n . Ident . (:[])) ps

-- booleans
_not :: Dict -> [WithPos PzVal] -> FuncReturn
_not ctx args = f1 args $ \x -> return $ (ctx,) $ case boolish x of
    FalseReal -> pzTrue
    Falsish -> pzTrue
    Truish -> pzFalse
    TrueReal -> pzFalse

_or :: Dict -> [WithPos PzVal] -> FuncReturn
_or ctx args = f2 args $ \x y -> return $ (ctx,) $
    case (boolish x, boolish y) of
        (TrueReal , _)            -> x
        (Truish   , TrueReal)     -> y
        (Truish   , _)            -> x
        (Falsish  , FalseReal)    -> x
        (Falsish  , _)            -> y
        (FalseReal, _)            -> y

_and :: Dict -> [WithPos PzVal] -> FuncReturn
_and ctx args = f2 args $ \x y -> return $ (ctx,) $
    case (boolish x, boolish y) of
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

-----------
-- Utils
-----------
data Boolish
    = FalseReal
    | Falsish
    | Truish
    | TrueReal
    deriving (Show, Eq)

boolish :: WithPos PzVal -> Boolish
boolish v
  | v == pzFalse = FalseReal
  | v == pzTrue = TrueReal
  | otherwise = case val v of
    PzUnit -> Falsish
    PzNum 0 -> Falsish
    PzStr "" -> Falsish
    PzList [] -> Falsish
    PzDict d -> if M.null d then Falsish else Truish
    _ -> Truish