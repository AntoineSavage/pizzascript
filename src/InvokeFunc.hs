module InvokeFunc where

import qualified Data.Map as M

import BuiltIns.Dispatch ( dispatch, dispatchQuoted )
import Ops.PzVal ( fromQuoted )
import Types.Func ( Func(..) )
import Types.Func.FuncArgs ( FuncArgs(..) )
import Types.Func.FuncBody ( FuncBody(..) )
import Types.Func.FuncImpureArgs ( FuncImpureArgs(..) )
import Types.PzVal ( Dict, DictKey(..), Evaled, PzFunc, PzVal(..), Quoted )
import Types.Symb ( Symb(..) )
import Utils ( Result, invalidArityMsg )

class ClsInvokeFunc a where
    clsDispatch :: Dict -> [PzVal a] -> String -> Result (PzVal Evaled)
    clsToEvaled :: PzVal a -> PzVal Evaled

instance ClsInvokeFunc Quoted where clsDispatch = dispatchQuoted; clsToEvaled = fromQuoted
instance ClsInvokeFunc Evaled where clsDispatch _ = dispatch; clsToEvaled = id

data InvokeFuncResult
    = ResultEvaled (PzVal Evaled)
    | ResultPushBlock (Dict, [PzVal Quoted])
    deriving (Show, Eq)

invokeFunc :: ClsInvokeFunc a => Dict -> Dict -> PzFunc -> [PzVal a] -> Result InvokeFuncResult
invokeFunc ctx implCtx (Func impArgs args body) vs = case body of
    BodyBuiltIn (Symb _ f cs) -> ResultEvaled <$> clsDispatch ctx vs (f:cs)
    BodyCustom e es -> do
        let (expLen, argImplCtx) = buildArgImplCtx ctx impArgs args $ map clsToEvaled vs
            finalImplCtx = M.union argImplCtx implCtx
        if length vs /= expLen
            then Left $ invalidArityMsg expLen vs
            else return $ ResultPushBlock (finalImplCtx, e:es)

buildArgImplCtx :: Dict -> FuncImpureArgs -> FuncArgs -> [PzVal Evaled] -> (Int, Dict)
buildArgImplCtx ctx impArgs args vs =
    let put acc (k, v) = M.insert (DictKey k) v acc

        explCtxPairs = case impArgs of
            Both _ i -> [ (PzSymb i, PzDict ctx) ]
            _ -> []
     
        (expLen, argPairs) = case args of
            ArgsVaria i -> (length vs, [ (PzSymb i, PzList vs) ])
            ArgsArity is -> (length is, zip (map PzSymb is) vs)

    in (expLen, foldl put M.empty $ explCtxPairs ++ argPairs)