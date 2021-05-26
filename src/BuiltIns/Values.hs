module BuiltIns.Values where

import qualified Data.Map as M

import Symbs ( symbAnd, symbArgs,  symbCtx, symbFalse, symbFunc, symbNot, symbOr, symbTrue, symbX, symbY )
import Types.Func ( Func(..) )
import Types.Func.ArgPass ( ArgPass(..) )
import Types.Func.FuncArgs ( FuncArgs(..) )
import Types.Func.FuncBody ( FuncBody(..) )
import Types.Func.FuncImpureArgs ( FuncImpureArgs(..) )
import Types.PzVal ( PzVal(..) )

-- numbers
-- TODO

-- strings
-- TODO

-- symbols
-- TODO

-- booleans
pzNot :: PzVal
pzNot = PzFunc M.empty $ Func None
    (ArgsArity [symbX])
    $ BodyBuiltIn symbNot

pzOr :: PzVal
pzOr = PzFunc M.empty $ Func None
    (ArgsArity [symbX, symbY])
    $ BodyBuiltIn symbOr

pzAnd :: PzVal
pzAnd = PzFunc M.empty $ Func None
    (ArgsArity [symbX, symbY])
    $ BodyBuiltIn symbAnd

-- lists
-- TODO

-- dictionaries
-- TODO

-- functions
pzFunc :: PzVal
pzFunc = PzFunc M.empty func

func :: Func a
func = Func
    (Both Quote symbCtx)
    (ArgsVaria symbArgs)
    $ BodyBuiltIn symbFunc

-- miscellaneous
-- TODO