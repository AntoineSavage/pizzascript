module BuiltIns.Dispatch where

import qualified BuiltIns.FuncImpls as Impls

import Types.PzVal ( Dict, Evaled, PzVal, Quoted )
import Types.Symb ( Symb(..) )
import Utils ( f1, f2, f3, Result )

dispatchQuoted :: Dict -> [PzVal Quoted] -> String -> Result (PzVal Evaled)
dispatchQuoted ctx args funcName = case funcName of
    -- functions
    "func" -> Impls._func ctx args

    _ -> error $ "Built-in function '" ++ funcName ++ "' not supported (dispatchQuoted)"

dispatch :: [PzVal Evaled] -> String -> Result (PzVal Evaled)
dispatch args funcName = case funcName of
    -- generic
    "type_of" -> f1 args $ Right . Impls._typeOf
    "eq" -> f2 args $ Right .* Impls._eq
    "lt" -> f2 args $ Right .* Impls._lt

    -- semi-generic
    "is_empty" -> f1 args Impls._isEmpty
    "size" -> f1 args Impls._size

    -- numbers
    "num" -> f1 args Impls._num
    "add" -> f2 args Impls._add
    "sub" -> f2 args Impls._sub
    "mult" -> f2 args Impls._mult
    "div" -> f2 args Impls._div
    "rem" -> f2 args Impls._rem
    "exp" -> f2 args Impls._exp
    "log" -> f2 args Impls._log
    "round" -> f1 args Impls._round
    "floor" -> f1 args Impls._floor
    "ceil" -> f1 args Impls._ceil
    "trunc" -> f1 args Impls._trunc

    -- strings
    "str" -> Right $ Impls._str args
    "split" -> f2 args Impls._split
    "join" -> Impls._join args

    -- symbols
    "symb" -> f1 args Impls._symb
    "nbr_quotes" -> f1 args Impls._nbrQuotes

    -- booleans
    "not" -> f1 args $ Right . Impls._not
    "or" -> f2 args $ Right .* Impls._or
    "and" -> f2 args $ Right .* Impls._and

    -- lists
    "cons" -> f2 args Impls._cons
    "head" -> f1 args Impls._head
    "tail" -> f1 args Impls._tail

    -- dictionaries
    "keys" -> f1 args Impls._keys
    "assocs" -> f1 args Impls._assocs
    "contains" -> f2 args Impls._contains
    "get" -> f2 args Impls._get
    "put" -> f3 args Impls._put
    "del" -> f2 args Impls._del

    -- functions
    "get_impl_ctx" -> f1 args Impls._getImplCtx
    "set_impl_ctx" -> f2 args Impls._setImplCtx
    "get_expl_ctx" -> f1 args Impls._getExplCtx
    "get_arg_pass" -> f1 args Impls._getArgPass
    "get_args" -> f1 args Impls._getArgs
    "get_body" -> f1 args Impls._getBody

    _ -> error $ "Built-in function '" ++ funcName ++ "' not supported (dispatch)"

(.*) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.*) f g x y = f $ g x y