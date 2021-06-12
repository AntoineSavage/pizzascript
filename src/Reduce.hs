module Reduce where

import qualified Data.Map as M

import Ops.Symb ( symb )
import Types.Func.FuncArgs ( FuncArgs(..) )
import Types.Func.FuncImpureArgs ( FuncImpureArgs(..) )
import Types.PzVal ( Dict, DictKey(..), Evaled, PzVal(..), Quoted )
import Types.StackFrame ( StackFrame )
import Utils ( Result )

--type AccResult = Result Acc
--type ReturnValue = Maybe (PzVal Evaled)
--data Acc
--    = Acc ReturnValue [StackFrame]
--    deriving (Show, Eq)

buildArgImplCtx :: Dict -> FuncImpureArgs -> FuncArgs -> [PzVal Evaled] -> (Int, Dict)
buildArgImplCtx explCtx impArgs args vs =
    let put acc (k, v) = M.insert (DictKey k) v acc

        explCtxPairs = case impArgs of
            Both _ i -> [ (PzSymb i, PzDict explCtx) ]
            _ -> []
     
        (expLen, argPairs) = case args of
            ArgsVaria i -> (length vs, [ (PzSymb i, PzList vs) ])
            ArgsArity is -> (length is, zip (map PzSymb is) vs)

    in (expLen, foldl put M.empty $ explCtxPairs ++ argPairs)
