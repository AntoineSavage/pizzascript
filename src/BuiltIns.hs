module BuiltIns where

import qualified Data.Map as M

import Data.Symb ( symb )
import Data.Numb ( Numb(Numb) )
import Data.Str ( Str(Str) )
import Data.WithPos ( WithPos(WithPos, val), Pos )
import Text.Parsec.Pos ( newPos )
import Types
import Utils

builtInPos :: Pos
builtInPos = newPos "<built-in>" 0 0

withPos :: a -> WithPos a
withPos = WithPos builtInPos

-- Built-in context
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
pzNot = withPos $ PzFunc $ Func M.empty None
    (ArgsArity builtInPos [withPos identX])
    $ BodyBuiltIn identNot

pzOr :: WithPos PzVal
pzOr = withPos $ PzFunc $ Func M.empty None
    (ArgsArity builtInPos $ map withPos [identX, identY])
    $ BodyBuiltIn identOr

pzAnd :: WithPos PzVal
pzAnd = withPos $ PzFunc $ Func M.empty None
    (ArgsArity builtInPos $ map withPos [identX, identY])
    $ BodyBuiltIn identAnd

-- lists
-- TODO

-- dictionaries
-- TODO

-- functions
pzFunc :: WithPos PzVal
pzFunc = withPos $ PzFunc func

func :: Func
func = Func M.empty
    (Both builtInPos (withPos Quote) (withPos identCtx))
    (ArgsVaria $ withPos identArgs)
    $ BodyBuiltIn identFunc

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
    PzNum (Numb 0) -> Falsish
    PzStr (Str "") -> Falsish
    PzList [] -> Falsish
    PzDict d -> if M.null d then Falsish else Truish
    _ -> Truish