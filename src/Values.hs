module Values where

import qualified Data.Map as M

import Data.Func ( Func(Func) )
import Data.Func.ArgPass ( ArgPass(Quote) )
import Data.Func.FuncArgs ( FuncArgs(ArgsVaria, ArgsArity) )
import Data.Func.FuncBody ( FuncBody(BodyBuiltIn) )
import Data.Func.FuncImpureArgs ( FuncImpureArgs(Both, None) )
import Data.PzVal ( PzVal(PzFunc, PzSymb) )
import Data.WithPos ( Pos, WithPos(WithPos) )
import Idents ( identAnd, identArgs, identCtx, identFunc, identNot, identOr, identX, identY )
import Symbs ( symbFalse, symbTrue )
import Text.Parsec.Pos ( newPos )

-- position
builtInPos :: Pos
builtInPos = newPos "<built-in>" 0 0

withPos :: a -> WithPos a
withPos = WithPos builtInPos

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
pzNot = withPos $ PzFunc M.empty $ Func None
    (ArgsArity builtInPos [withPos identX])
    $ BodyBuiltIn identNot

pzOr :: WithPos PzVal
pzOr = withPos $ PzFunc M.empty $ Func None
    (ArgsArity builtInPos $ map withPos [identX, identY])
    $ BodyBuiltIn identOr

pzAnd :: WithPos PzVal
pzAnd = withPos $ PzFunc M.empty $ Func None
    (ArgsArity builtInPos $ map withPos [identX, identY])
    $ BodyBuiltIn identAnd

-- lists
-- TODO

-- dictionaries
-- TODO

-- functions
pzFunc :: WithPos PzVal
pzFunc = withPos $ PzFunc M.empty func

func :: Func
func = Func
    (Both builtInPos (withPos Quote) (withPos identCtx))
    (ArgsVaria $ withPos identArgs)
    $ BodyBuiltIn identFunc

-- miscellaneous
-- TODO