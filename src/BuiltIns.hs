{-# LANGUAGE TupleSections #-}
module BuiltIns where

import qualified Data.Map as M

import Text.Parsec.Pos ( newPos )
import Types
import Utils

-- Built-in context
pos :: Pos
pos = newPos "<built-in>" 0 0

withPos :: a -> WithPos a
withPos = WithPos pos

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
    , (PzSymb $ symb identNot, pzNot)
    , (PzSymb $ symb identOr, pzOr)
    , (PzSymb $ symb identAnd, pzAnd)

    -- lists
    -- TODO

    -- dictionaries
    -- TODO

    -- functions
    , (PzSymb $ symb identFunc, pzFunc)

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
pzFalse :: PzVal
pzFalse = PzSymb symbFalse

pzTrue :: PzVal
pzTrue = PzSymb symbTrue

pzNot :: PzVal
pzNot = PzFunc $ Func M.empty Nothing Eval (ArgsArity [identX]) $ BodyBuiltIn identNot

pzOr :: PzVal
pzOr = PzFunc $ Func M.empty Nothing Eval (ArgsArity [identX, identY]) $ BodyBuiltIn identOr

pzAnd :: PzVal
pzAnd = PzFunc $ Func M.empty Nothing Eval (ArgsArity [identX, identY]) $ BodyBuiltIn identAnd

-- lists
-- TODO

-- dictionaries
-- TODO

-- functions
pzFunc :: PzVal
pzFunc = PzFunc $ Func M.empty (Just identCtx) Quote (ArgsVaria identArgs) $ BodyBuiltIn identFunc

-- miscellaneous
-- TODO

-------------
-- Implems
-------------

type FuncReturn = Either String (Dict, PzVal)

-- numbers
-- TODO

-- strings
-- TODO

-- symbols
symbSplitImpl :: Symb -> [Symb]
symbSplitImpl (Symb n (Ident ps)) = map (Symb n . Ident . (:[])) ps

-- booleans
_not :: Dict -> [PzVal] -> FuncReturn
_not ctx args = f1 args $ \x -> return $ (ctx,) $ case boolish x of
    FalseReal -> pzTrue
    Falsish -> pzTrue
    Truish -> pzFalse
    TrueReal -> pzFalse

_or :: Dict -> [PzVal] -> FuncReturn
_or ctx args = f2 args $ \x y -> return $ (ctx,) $
    case (boolish x, boolish y) of
        (TrueReal , _)            -> x
        (Truish   , TrueReal)     -> y
        (Truish   , _)            -> x
        (Falsish  , FalseReal)    -> x
        (Falsish  , _)            -> y
        (FalseReal, _)            -> y

_and :: Dict -> [PzVal] -> FuncReturn
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