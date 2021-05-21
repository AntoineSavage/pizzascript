module Reduce where

import qualified Data.Map as M

import BuiltIns ( FuncReturn, builtInCtx, func, _not, _or, _and )
import Control.Monad ( forM_, liftM2 )
import Data.ArgPass ( ArgPass(..) )
import Data.AstExpr ( AstExpr, parseExpr )
import Data.Func ( Func(Func, impArgs), getArgPass )
import Data.FuncArgs ( FuncArgs(..) )
import Data.FuncBody ( FuncBody(..) )
import Data.FuncCustom ( fromFuncCustom )
import Data.FuncImpureArgs ( FuncImpureArgs(..) )
import Data.Ident ( Ident(..) )
import Data.Lst ( parseMany )
import Data.Maybe ( fromMaybe )
import Data.Nat ( Nat(..) )
import Data.Symb ( symb )
import Data.WithPos ( WithPos(..), Pos )
import Eval ( ExprEvalResult(..), evalExpr, evalFuncCustom )
import Quote ( quote, unquote )
import Text.Parsec ( eof )
import Text.Parsec.String ( parseFromFile )
import Types
import Utils

type EvalResult = Either String Acc

evalFrame :: Result -> StackFrame -> [StackFrame] -> EvalResult
evalFrame result frame frames =
    case frame of
        Block ctx es -> evalBlock result ctx es frames
        Form ctx p mfi es -> evalForm result ctx p mfi es frames
        Invoc ctx p fi ic f as es -> evalInvoc result ctx p fi ic f as es frames

evalBlock :: Result -> Dict -> [WithPos AstExpr] -> [StackFrame] -> EvalResult
evalBlock result ctx es frames =
    case es of
        [] ->
            -- block finished: pop frame
            return $ Acc result frames
       
        e:es ->
            -- evaluate next block expression
            evalExpr ctx e Eval >>= toAcc ctx e (Block ctx es : frames)

evalForm :: Result -> Dict -> Pos -> Maybe (WithPos Ident) -> [WithPos AstExpr] -> [StackFrame] -> EvalResult
evalForm result ctx p mfi elems frames =
    case result of
        Nothing ->
            -- no result to process yet
            case elems of
                [] ->
                    -- empty form -> return unit type (and pop frame)
                    return $ Acc (Just $ WithPos p PzUnit) frames
       
                e:es ->
                    -- evaluate first form element (should be a function)
                    evalExpr ctx e Eval >>= toAcc ctx e (Form ctx p mfi es : frames)
       
        Just f ->
            -- process result (first form elem, should be a function)
            case val f of
                PzFunc ic f ->

                    -- handle 'func if needed (reduces number of built-in dependencies)
                    if f == func
                        then case invokeFuncSpecial ctx p elems frames of
                            Left s -> Left $ addIdentAndPos p mfi s
                            Right acc -> return acc

                        -- replace form with function invocation
                        else  return $ Acc Nothing $ Invoc ctx p mfi ic f [] (Just elems) : frames

                _ -> Left $
                    "Error: Malformed function invocation (first form element must be a function)"
                    ++ fromMaybe "" (flip fmap mfi $ \fi -> "\n when invoking function: " ++ show fi)
                    ++ "\n at: " ++ show p
                    ++ "\n" ++ show f

evalInvoc :: Result -> Dict -> Pos -> Maybe (WithPos Ident) -> Dict -> Func -> [WithPos PzVal] -> Maybe [WithPos AstExpr] -> [StackFrame] -> EvalResult
evalInvoc result ctx p mfi implCtx f as melems frames =
    case result of
        Nothing ->
            -- no result to process yet
            case melems of
                Nothing ->
                    -- marked for invocation: invoke function
                    case invokeFunc ctx p implCtx f as frames of
                        Left s -> Left $ addIdentAndPos p mfi s
                        Right acc -> return acc

                Just [] ->
                    -- all args evaluated: mark for invocation
                    return $ Acc Nothing $ Invoc ctx p mfi implCtx f (reverse as) Nothing : frames

                Just (e:es) ->
                    -- evaluate function argument according to argument-passing behaviour
                    case evalExpr ctx e (getArgPass f) >>= toAcc ctx e (Invoc ctx p mfi implCtx f as (Just es) : frames) of
                        Left s -> Left $ addIdentAndPos p mfi s
                        Right acc -> return acc

        Just r ->
            -- process result
            case melems of
                Just es ->
                    -- argument evaluation result
                    return $ Acc Nothing $ Invoc ctx p mfi implCtx f (r:as) (Just es) : frames

                _ ->
                    -- function invocation result (pop frame)
                    case impArgs f of
                        Both {} -> case r of
                            -- Impure functions: handle explicit output context
                            WithPos _ (PzList [WithPos _ (PzDict ctx'), r']) ->
                                return $ Acc (Just r') $ setCtx ctx' frames

                            _ -> Left $ addIdentAndPos p mfi $
                                "Error: Invalid impure function return value. Must be a size-2 list containing (in order):"
                                ++ "\n 1) the output context (a dictionary)"
                                ++ "\n 2) the normal return value (any type)"
                                ++ "\n was: " ++ show r

                        _ -> return $ Acc (Just r) frames

invokeFuncSpecial :: Dict -> Pos -> [WithPos AstExpr] -> [StackFrame] -> EvalResult
invokeFuncSpecial ctx p es frames = do
    fc <- evalFuncCustom es
    let r = WithPos p $ PzFunc ctx $ fromFuncCustom fc
    return $ Acc (Just r) frames

invokeFunc :: Dict -> Pos -> Dict -> Func -> [WithPos PzVal] -> [StackFrame] -> EvalResult
invokeFunc ctx p implCtx (Func impArgs args body) as frames =
    case body of
        BodyBuiltIn ident -> invokeFuncBuiltIn ctx p as ident frames
        BodyCustom es -> invokeFuncCustom ctx p as implCtx impArgs args es frames

invokeFuncCustom :: Dict -> Pos -> [WithPos PzVal] -> Dict -> FuncImpureArgs -> FuncArgs -> [WithPos AstExpr] -> [StackFrame] -> EvalResult
invokeFuncCustom explCtx p as implCtx impArgs args es frames =
    let toExpr = fmap $ PzSymb .symb
        explCtxPairs = case impArgs of
            Both _ _ i -> [ (toExpr i, WithPos (pos i) $ PzDict explCtx) ]
            _ -> []
       
        actLen = length as
        (expLen, argPairs) = case args of
            ArgsVaria i -> (actLen, [ (toExpr i, WithPos (pos i) $ PzList as) ])
            ArgsArity _ is -> (length is, zip (map toExpr is) as)

        pairs = explCtxPairs ++ argPairs
        f acc (k, v) = M.insert k v acc
        finalImplCtx = foldl f implCtx pairs

    in if actLen == expLen
            then return $ Acc Nothing $ Block finalImplCtx es : frames
            else Left $
                "Error: Invoking function with incorrect number of arguments:"
                ++ "\n expected: " ++ show expLen
                ++ "\n received: " ++ show actLen

-- Utils
toAcc :: Dict -> WithPos AstExpr -> [StackFrame] -> ExprEvalResult -> EvalResult
toAcc ctx e frames r = case r of
    Evaled v -> return $ Acc (Just v) frames
    ExprForm p es ->
        let mfi = case getIdent e of Left _ -> Nothing; Right fi -> Just fi in
        return $ Acc Nothing $ Form ctx p mfi es : frames

returnFrom :: [StackFrame] -> FuncReturn -> EvalResult
returnFrom frames x = x >>= \(ctx, r) -> return $ Acc (Just r) $ setCtx ctx frames

-- Built-in functions
-- See module BuiltIns for implementations
invokeFuncBuiltIn :: Dict -> Pos -> [WithPos PzVal] -> Ident -> [StackFrame] -> EvalResult
invokeFuncBuiltIn ctx p args (Ident s) frames =
    case s of
        -- numbers
        -- TODO

        -- strings
        -- TODO

        -- symbols
        -- TODO

        -- booleans
        "not" -> returnFrom frames $ f1 args $ \x -> fpure ctx $ _not x
        "or" -> returnFrom frames $ f2 args $ \x y -> fpure ctx $ _or x y
        "and" -> returnFrom frames $ f2 args $ \x y -> fpure ctx $ _and x y

        -- lists
        -- TODO

        -- dictionaries
        -- TODO

        -- functions
        -- TODO

        -- miscellaneous
        -- TODO

        _ -> Left $ "TODO: Implement built-in function: " ++ show s