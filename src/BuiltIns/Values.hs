module BuiltIns.Values where

import qualified Data.Map as M

import Data.Func ( Func(..) )
import Data.Func.ArgPass ( ArgPass(..) )
import Data.Func.FuncArgs ( FuncArgs(..) )
import Data.Func.FuncBody ( FuncBody(..) )
import Data.Func.FuncImpureArgs ( FuncImpureArgs(..) )
import Data.PzVal ( PzVal(..) )
import Symbs ( symbAnd, symbArgs,  symbCtx, symbFalse, symbFunc, symbNot, symbOr, symbTrue, symbX, symbY )

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