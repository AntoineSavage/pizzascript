module BuiltIns.Dispatch where

import qualified BuiltIns.FuncImpls as Impls

import Types.PzVal ( Dict, PzVal )
import Types.Symb ( Symb(..) )
import Utils ( f1, f2, f3, fpure, Result )

type FuncResult = Result (Dict, PzVal)

dispatch :: Dict -> [PzVal] -> String -> FuncResult
dispatch ctx args funcName = case funcName of
    -- generic
    "type_of" -> f1 args $ \x -> fpure ctx $ Impls._typeOf x
    "eq" -> f2 args $ \x y -> fpure ctx $ Impls._eq x x
    "lt" -> f2 args $ \x y -> fpure ctx $ Impls._lt x x

    -- semi-generic
    "is_empty" -> f1 args $ \x -> fpure ctx $ Impls._isEmpty x
    "size" -> f1 args $ \x -> fpure ctx $ Impls._size x

    -- numbers
    "num" -> f1 args $ \x -> fpure ctx $ Impls._num x
    "add" -> f2 args $ \x y -> fpure ctx $ Impls._add x y
    "sub" -> f2 args $ \x y -> fpure ctx $ Impls._sub x y
    "mult" -> f2 args $ \x y -> fpure ctx $ Impls._mult x y
    "div" -> f2 args $ \x y -> fpure ctx $ Impls._div x y
    "rem" -> f2 args $ \x y -> fpure ctx $ Impls._rem x y
    "exp" -> f2 args $ \x y -> fpure ctx $ Impls._exp x y
    "log" -> f2 args $ \x y -> fpure ctx $ Impls._log x y
    "round" -> f1 args $ \x -> fpure ctx $ Impls._round x
    "floor" -> f1 args $ \x -> fpure ctx $ Impls._floor x
    "ceil" -> f1 args $ \x -> fpure ctx $ Impls._ceil x
    "trunc" -> f1 args $ \x -> fpure ctx $ Impls._trunc x

    -- strings
    "str" -> fpure ctx $ Impls._str args
    "split" -> f2 args $ \x y -> fpure ctx $ Impls._split x y
    "join" -> fpure ctx $ Impls._join args

    -- symbols
    "symb" -> f1 args $ \x -> fpure ctx $ Impls._symb x
    "nbr_quotes" -> f1 args $ \x -> fpure ctx $ Impls._nbrQuotes x

    -- booleans
    "not" -> f1 args $ \x -> fpure ctx $ Impls._not x
    "or" -> f2 args $ \x y -> fpure ctx $ Impls._or x y
    "and" -> f2 args $ \x y -> fpure ctx $ Impls._and x y

    -- lists
    "cons" -> f2 args $ \x y -> fpure ctx $ Impls._cons x y
    "head" -> f1 args $ \x -> fpure ctx $ Impls._head x
    "tail" -> f1 args $ \x -> fpure ctx $ Impls._tail x

    -- dictionaries
    "keys" -> f1 args $ \x -> fpure ctx $ Impls._keys x
    "assocs" -> f1 args $ \x -> fpure ctx $ Impls._assocs x
    "contains" -> f2 args $ \x y -> fpure ctx $ Impls._contains x y
    "get" -> f2 args $ \x y -> fpure ctx $ Impls._get x y
    "put" -> f3 args $ \x y z -> fpure ctx $ Impls._put x y z
    "del" -> f2 args $ \x y -> fpure ctx $ Impls._del x y

    -- functions
    "get_impl_ctx" -> f1 args $ \x -> fpure ctx $ Impls._getImplCtx x
    "set_impl_ctx" -> f2 args $ \x y -> fpure ctx $ Impls._setImplCtx x y
    "get_expl_ctx" -> f1 args $ \x -> fpure ctx $ Impls._getExplCtx x
    "get_arg_pass" -> f1 args $ \x -> fpure ctx $ Impls._getArgPass x
    "get_args" -> f1 args $ \x -> fpure ctx $ Impls._getArgs x
    "get_body" -> f1 args $ \x -> fpure ctx $ Impls._getBody x

    _ -> error $ "Built-in function '" ++ funcName ++ "' not supported"