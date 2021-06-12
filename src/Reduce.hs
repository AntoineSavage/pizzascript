{-# LANGUAGE LambdaCase #-}
module Reduce where

import qualified Data.Map as M

import BuiltIns.Dispatch ( dispatch, dispatchQuoted )
import Ops.PzVal ( fromQuoted )
import Ops.Symb ( symb, unparseSymb )
import Ops.StackFrame ( setCtx )
import Types.Func ( Func(..) )
import Types.Func.FuncArgs ( FuncArgs(..) )
import Types.Func.FuncBody ( FuncBody(..) )
import Types.Func.FuncImpureArgs ( FuncImpureArgs(..) )
import Types.PzVal ( Dict, DictKey(..), Evaled, PzFunc, PzVal(..), Quoted )
import Types.Symb ( Symb(..) )
import Types.StackFrame ( StackFrame(..), StackFrameSpec(..) )
import Utils ( Result, invalidArityMsg )

type ReturnValue = Maybe (PzVal Evaled)
data Acc
    = Acc ReturnValue [StackFrame]
    deriving (Show, Eq)

reduceInvoc :: Dict -> Dict -> PzFunc -> [PzVal Evaled] -> Maybe [PzVal Quoted] -> Acc -> Result Acc
reduceInvoc ctx implCtx f as mes (Acc rval frames) = case rval of

    -- no return value to process: evaluate arg or invoke function
    Nothing -> case mes of

        -- evaluate arg according to argument-passing behaviour
        -- TODO
        --case eval ctx e (getArgPass f) >>= toAcc ctx e (Invoc ctx mfi implCtx f as (Just es) : frames) of
        --    Left s -> Left $ addIdentAndPos mfi s
        --    Right acc -> return acc
        Just (e:es) -> Left "Not implemented: evaluate arg according to argument-passing behaviour"

        -- all args evaluated: mark for invocation
        Just [] -> return $ Acc Nothing $ StackFrame ctx (InvocEvaled implCtx f (reverse as) Nothing) : frames

        -- marked for invocation: invoke function
        Nothing -> case invokeFunc ctx implCtx f as of

            -- function invocation error
            Left s -> Left s

            -- function invocation result
            Right fr -> case fr of

                -- built-in function: return value and pop frame
                ResultBuiltIn r -> return $ Acc (Just r) frames

                -- custom function: push frame
                ResultCustom (ctx', es) -> return $ Acc Nothing $ StackFrame ctx' (Block es) : frames

    -- process return value: arg evaluation or function invokation
    Just r -> case mes of

        -- arg evaluation return value
        Just es -> return $ Acc Nothing $ StackFrame ctx (InvocEvaled implCtx f (r:as) $ Just es) : frames

        -- function invocation return value: handle impure function return value
        _ -> case impArgs f of

            -- Pure function: normal output format
            None -> return $ Acc (Just r) frames
            ArgPass _ -> return $ Acc (Just r) frames

            -- Impure function: special output format:
            --   size-2 list of: ctx, and normal return value
            _ -> case r of

                -- Valid output
                PzList [PzDict ctx', r'] -> return $ Acc (Just r') $ setCtx ctx' frames

                -- Invalid output
                _ -> Left $
                    "Error: Invalid impure function return value. Must be a size-2 list containing (in order):"
                    ++ "\n 1) the output context (a dictionary)"
                    ++ "\n 2) the normal return value (any type)"
                    ++ "\n was: " ++ show r

class ClsInvokeFunc a where
    clsDispatch :: Dict -> [PzVal a] -> String -> Result (PzVal Evaled)
    clsToEvaled :: PzVal a -> PzVal Evaled

instance ClsInvokeFunc Quoted where clsDispatch = dispatchQuoted; clsToEvaled = fromQuoted
instance ClsInvokeFunc Evaled where clsDispatch _ = dispatch; clsToEvaled = id

data InvokeFuncResult
    = ResultBuiltIn (PzVal Evaled)
    | ResultCustom (Dict, [PzVal Quoted])
    deriving (Show, Eq)

invokeFunc :: ClsInvokeFunc a => Dict -> Dict -> PzFunc -> [PzVal a] -> Result InvokeFuncResult
invokeFunc ctx implCtx (Func impArgs args body) vs = case body of
    BodyBuiltIn (Symb _ f cs) -> ResultBuiltIn <$> clsDispatch ctx vs (f:cs)
    BodyCustom e es -> do
        let (expLen, argImplCtx) = buildArgImplCtx ctx impArgs args $ map clsToEvaled vs
            finalImplCtx = M.union argImplCtx implCtx
        if length vs /= expLen
            then Left $ invalidArityMsg expLen vs
            else return $ ResultCustom (finalImplCtx, e:es)

-- Utils
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