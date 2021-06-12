{-# LANGUAGE LambdaCase #-}
module Reduce where

import Eval ( EvalResult(..), eval )
import InvokeFunc ( InvokeFuncResult(..), ClsInvokeFunc, invokeFunc )
import Ops.Func ( getArgPass )
import Ops.StackFrame ( setCtx )
import Types.Func ( Func(..) )
import Types.Func.ArgPass ( ArgPass(..) )
import Types.Func.FuncImpureArgs ( FuncImpureArgs(..) )
import Types.PzVal ( Dict, Evaled, PzFunc, PzVal(..), Quoted )
import Types.StackFrame ( StackFrame(..), StackFrameSpec(..) )
import Utils ( Result )

type ReturnValue = Maybe (PzVal Evaled)
data Acc
    = Acc ReturnValue [StackFrame]
    deriving (Show, Eq)

reduce :: Acc -> Result Acc
reduce acc@(Acc _ []) = return acc -- nothing left to reduce
reduce (Acc rval (StackFrame ctx spec :frames)) =
    -- pre-pop frame here: afterwards 'pop frame' means not pushing new frame
    let acc = Acc rval frames in case spec of
        Block vs -> reduceBlock ctx vs acc
        FormQuoted v vs -> reduceFormQuoted ctx v vs acc
        FormEvaled v vs -> reduceFormEvaled ctx v vs acc
        InvocQuoted ic f vs -> reduceInvoc ctx ic f vs frames
        InvocArgs ic f vs qvs -> reduceInvocArgs ctx ic f vs qvs acc
        InvocEvaled ic f vs -> reduceInvocEvaled ctx ic f vs acc

reduceBlock :: Dict -> [PzVal Quoted] -> Acc -> Result Acc
reduceBlock ctx vs acc@(Acc _ frames) = case vs of

    -- block is finished: pop frame
    [] -> return acc

    -- evaluate next block element
    v:vs -> eval ctx v >>= toAcc ctx frames (Block vs)

reduceFormQuoted :: Dict -> PzVal Quoted -> [PzVal Quoted] -> Acc -> Result Acc
reduceFormQuoted ctx v vs (Acc rval frames) = case rval of

    -- no return value: eval form first element (function)
    Nothing -> eval ctx v >>= toAcc ctx frames (FormQuoted v vs)

    -- return value: form first element (function) is evaled
    Just r -> return $ Acc Nothing $ StackFrame ctx (FormEvaled r vs) : frames

reduceFormEvaled :: Dict -> PzVal Evaled -> [PzVal Quoted] -> Acc -> Result Acc
reduceFormEvaled ctx v vs (Acc _ frames) = case v of

    -- replace form with function invocation
    PzFunc ic f -> return $ Acc Nothing $ (:frames) $ StackFrame ctx $ case getArgPass f of

        -- Skip evaluating and quoting each argument; invokes function directly
        Quote -> InvocQuoted ic f vs

        -- Will evaluate (and possibly quote/unquote) each argument before invocation
        _ -> InvocArgs ic f [] vs

    _ -> Left $
        "Error: Malformed function invocation (first form element must be a function)"
        ++ "\n was: " ++ show v

reduceInvocArgs :: Dict -> Dict -> PzFunc -> [PzVal Evaled] -> [PzVal Quoted] -> Acc -> Result Acc
reduceInvocArgs ctx ic f vs qvs (Acc rval frames) = case rval of

    -- no return value: eval args
    Nothing -> case qvs of

        -- all args evaled: mark for invocation
        [] -> return $ Acc Nothing $ StackFrame ctx (InvocEvaled ic f $ reverse vs) : frames

        -- eval arg according to arg-pass behaviour
        e:es -> Left "Not implemented: evaluate arg according to argument-passing behaviour"

    -- return value: arg evaled
    Just v -> return $ Acc Nothing $ StackFrame ctx (InvocArgs ic f (v:vs) qvs) : frames

reduceInvocEvaled :: Dict -> Dict -> PzFunc -> [PzVal Evaled] -> Acc -> Result Acc
reduceInvocEvaled ctx ic f vs acc@(Acc rval frames) = case rval of

    -- no return value to process: invoke function
    Nothing -> reduceInvoc ctx ic f vs $ StackFrame ctx (InvocEvaled ic f vs) : frames

    -- return value: handle pure/impure output (pop frame)
    Just r -> case impArgs f of

        -- Impure function: special output format:
        --   size-2 list of: ctx, and normal return value
        Both {} -> case r of

            -- Valid output
            PzList [PzDict ctx', r'] -> return $ Acc (Just r') $ setCtx ctx' frames

            -- Invalid output
            _ -> Left $
                "Error: Invalid impure function output. Must be a size-2 list containing (in order):"
                ++ "\n 1) the output context (a dictionary)"
                ++ "\n 2) the normal return value (any type)"
                ++ "\n was: " ++ show r

        -- Pure function: normal output format
        _ -> return acc

reduceInvoc :: ClsInvokeFunc a => Dict -> Dict -> PzFunc -> [PzVal a] -> [StackFrame] -> Result Acc
reduceInvoc ctx ic f vs frames = invokeFunc ctx ic f vs >>= \case

    -- built-in function: return value and pop frame
    ResultEvaled r -> return $ Acc (Just r) frames

    -- custom function: push frame
    ResultPushBlock (ctx', es) -> return $ Acc Nothing $ StackFrame ctx' (Block es) : frames

-- Utils
toAcc :: Dict -> [StackFrame] -> StackFrameSpec -> EvalResult -> Result Acc
toAcc ctx frames spec r = let frames' = StackFrame ctx spec :frames in case r of
    Evaled v -> return $ Acc (Just v) frames'
    PushForm v vs -> return $ Acc Nothing $ StackFrame ctx (FormQuoted v vs) :frames'