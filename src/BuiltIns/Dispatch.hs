module BuiltIns.Dispatch where

import qualified BuiltIns.Impls as Impls

import Types.PzVal ( Dict, PzVal )
import Types.Symb ( Symb(..) )
import Utils ( f1, f2, fpure, Result )

type FuncResult = Result (Dict, PzVal)

dispatch :: Dict -> [PzVal] -> String -> FuncResult
dispatch ctx args funcName = case funcName of
    -- numbers
    -- TODO

    -- strings
    -- TODO

    -- symbols
    -- TODO

    -- booleans
    "not" -> f1 args $ \x -> fpure ctx $ Impls._not x
    "or" -> f2 args $ \x y -> fpure ctx $ Impls._or x y
    "and" -> f2 args $ \x y -> fpure ctx $ Impls._and x y

    -- lists
    -- TODO

    -- dictionaries
    -- TODO

    -- functions
    -- TODO

    -- miscellaneous
    -- TODO

    _ -> error $ "Built-in function '" ++ funcName ++ "' not implemented"