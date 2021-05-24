module BuiltIns.Values where

import qualified Data.Map as M

import Data.Func ( Func(Func) )
import Data.Func.ArgPass ( ArgPass(Quote) )
import Data.Func.FuncArgs ( FuncArgs(ArgsVaria, ArgsArity) )
import Data.Func.FuncBody ( FuncBody(BodyBuiltIn) )
import Data.Func.FuncImpureArgs ( FuncImpureArgs(Both, None) )
import Data.PzVal ( PzVal(PzFunc, PzSymb) )
import Idents ( identAnd, identArgs, identCtx, identFunc, identNot, identOr, identX, identY )
import Symbs ( symbFalse, symbTrue )

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
pzNot = PzFunc M.empty $ Func None
    (ArgsArity [identX])
    $ BodyBuiltIn identNot

pzOr :: PzVal
pzOr = PzFunc M.empty $ Func None
    (ArgsArity [identX, identY])
    $ BodyBuiltIn identOr

pzAnd :: PzVal
pzAnd = PzFunc M.empty $ Func None
    (ArgsArity [identX, identY])
    $ BodyBuiltIn identAnd

-- lists
-- TODO

-- dictionaries
-- TODO

-- functions
pzFunc :: PzVal
pzFunc = PzFunc M.empty func

func :: Func
func = Func
    (Both (Quote) (identCtx))
    (ArgsVaria $ identArgs)
    $ BodyBuiltIn identFunc

-- miscellaneous
-- TODO