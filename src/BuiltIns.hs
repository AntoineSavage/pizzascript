module BuiltIns where

import qualified Data.Map as M

import Types
import Utils

{- TODO for base:
    - not, or, and
    - is_empty, size (list / dict)
    - cons, head, tail
    - keys, get, put, del
    - def
    - func
-}
-- Built-in context
builtInCtx :: Dict
builtInCtx = M.fromList
    [ (pzFalse, pzFalse)
    , (pzTrue, pzTrue)
    , (PzSymb $ symb $ ident "not", pzNot)
    ]

-- values
pzFalse :: PzVal
pzFalse = PzSymb $ symb identFalse

pzTrue :: PzVal
pzTrue = PzSymb $ symb identTrue

pzNot :: PzVal
pzNot = PzFunc $ Func Eval Nothing (ArgsArity [ident "x"]) $ BodyBuiltIn "_not"

-- functions
type FuncSig = Dict -> [PzVal] -> FuncReturn

_not :: FuncSig
_not ctx args = f1 args $ \x -> return $ (,) ctx $ if boolish x then pzFalse else pzTrue