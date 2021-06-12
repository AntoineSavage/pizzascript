-- Main
module Main where

import qualified Data.Map as M

import BuiltIns.Ctx ( builtInCtx )
import Control.Monad ( forM_, liftM2 )
import Data.AstExpr ( parseExpr )
import Data.Lst ( parseMany )
import Data.Maybe ( fromMaybe )
import Data.Nat ( Nat(..) )
import Data.StackFrame ( StackFrame(..) )
import Ignore ( ignore )
import Quote ( quote, unquote )
import Text.Parsec ( eof )
import Text.Parsec.String ( parseFromFile )
import Reduce ( Acc(..), evalFrame )

main :: IO ()
main = do
    let parser = parseMany ignore parse eof
        parse = parseExpr ignore parse
    mes <- parseFromFile parser "example/main.pz"
    case mes of
        Left err -> print err
        Right es -> go $ Acc Nothing [Block builtInCtx es]

go :: Acc -> IO ()
go (Acc result []) = putStrLn "Halting" -- no more frames: halt
go (Acc result (frame:frames)) = do
    putStrLn "=========="
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Frame: " ++ show frame
    putStrLn $ "Nbr additional frames: " ++ show (length frames)
    putStrLn "----------"
    case evalFrame result frame frames of
        Left s -> putStrLn s
        Right acc -> go acc

-- Reduce
module Reduce where

import qualified Data.Map as M

import BuiltIns.Ctx ( builtInCtx )
import BuiltIns.Impls ( _not, _or, _and )
import BuiltIns.Values ( func )
import Control.Monad ( forM_, liftM2 )
import Data.AstExpr ( AstExpr, parseExpr )
import Data.Func ( Func(Func, impArgs), getArgPass )
import Data.Func.ArgPass ( ArgPass(..) )
import Data.Func.FuncArgs ( FuncArgs(..) )
import Data.Func.FuncBody ( FuncBody(..) )
import Data.Func.FuncCustom ( fromFuncCustom )
import Data.Func.FuncImpureArgs ( FuncImpureArgs(..) )
import Data.Ident ( Ident(..) )
import Data.Lst ( parseMany )
import Data.Maybe ( fromMaybe )
import Data.Nat ( Nat(..) )
import Data.PzVal ( Dict, PzVal(..) )
import Data.StackFrame ( StackFrame(..), setCtx )
import Data.Symb ( symb )
import Eval ( ExprEvalResult(..), evalExpr, evalFuncCustom )
import Quote ( quote, unquote )
import Text.Parsec ( eof )
import Text.Parsec.String ( parseFromFile )
import Utils ( Result, addIdentAndPos, f1, f2, fpure, getIdent )

evalFrame :: ReturnValue -> StackFrame -> [StackFrame] -> EvalResult
evalFrame rval frame frames =
    case frame of
        Block ctx es -> evalBlock rval ctx es frames
        Form ctx mfi es -> evalForm rval ctx mfi es frames
        Invoc ctx fi ic f as es -> evalInvoc rval ctx fi ic f as es frames

evalBlock :: ReturnValue -> Dict -> [AstExpr] -> [StackFrame] -> EvalResult
evalBlock rval ctx es frames =
    case es of
        [] ->
            -- block finished: pop frame
            return $ Acc rval frames
     
        e:es ->
            -- evaluate next block expression
            evalExpr ctx e Eval >>= toAcc ctx e (Block ctx es : frames)

evalForm :: ReturnValue -> Dict -> Maybe Ident -> [AstExpr] -> [StackFrame] -> EvalResult
evalForm rval ctx mfi elems frames =
    case rval of
        Nothing ->
            -- no return value to process yet
            case elems of
                [] ->
                    -- empty form -> return unit type (and pop frame)
                    return $ Acc (Just PzUnit) frames
     
                e:es ->
                    -- evaluate first form element (should be a function)
                    evalExpr ctx e Eval >>= toAcc ctx e (Form ctx mfi es : frames)
     
        Just f ->
            -- process return value (first form elem, should be a function)
            case f of
                PzFunc ic f ->

                    -- handle 'func if needed (reduces number of built-in dependencies)
                    if f == func
                        then case invokeFuncSpecial ctx elems frames of
                            Left s -> Left $ addIdentAndPos mfi s
                            Right acc -> return acc

                        -- replace form with function invocation
                        else  return $ Acc Nothing $ Invoc ctx mfi ic f [] (Just elems) : frames

                _ -> Left $
                    "Error: Malformed function invocation (first form element must be a function)"
                    ++ fromMaybe "" (flip fmap mfi $ \fi -> "\n when invoking function: " ++ show fi)
                    ++ "\n" ++ show f

evalInvoc :: ReturnValue -> Dict -> Maybe Ident -> Dict -> Func -> [PzVal] -> Maybe [AstExpr] -> [StackFrame] -> EvalResult
evalInvoc rval ctx mfi implCtx f as melems frames =
    case rval of
        Nothing ->
            -- no return value to process yet
            case melems of
                Nothing ->
                    -- marked for invocation: invoke function
                    case invokeFunc ctx implCtx f as frames of
                        Left s -> Left $ addIdentAndPos mfi s
                        Right acc -> return acc

                Just [] ->
                    -- all args evaluated: mark for invocation
                    return $ Acc Nothing $ Invoc ctx mfi implCtx f (reverse as) Nothing : frames

                Just (e:es) ->
                    -- evaluate function argument according to argument-passing behaviour
                    case evalExpr ctx e (getArgPass f) >>= toAcc ctx e (Invoc ctx mfi implCtx f as (Just es) : frames) of
                        Left s -> Left $ addIdentAndPos mfi s
                        Right acc -> return acc

        Just r ->
            -- process return value
            case melems of
                Just es ->
                    -- argument evaluation return value
                    return $ Acc Nothing $ Invoc ctx mfi implCtx f (r:as) (Just es) : frames

                _ ->
                    -- function invocation return value (pop frame)
                    case impArgs f of
                        Both {} -> case r of
                            -- Impure functions: handle explicit output context
                            PzList [PzDict ctx', r'] ->
                                return $ Acc (Just r') $ setCtx ctx' frames

                            _ -> Left $ addIdentAndPos mfi $
                                "Error: Invalid impure function return value. Must be a size-2 list containing (in order):"
                                ++ "\n 1) the output context (a dictionary)"
                                ++ "\n 2) the normal return value (any type)"
                                ++ "\n was: " ++ show r

                        _ -> return $ Acc (Just r) frames

-- Utils
toAcc :: Dict -> AstExpr -> [StackFrame] -> ExprEvalResult -> EvalResult
toAcc ctx e frames r = case r of
    Evaled v -> return $ Acc (Just v) frames
    ExprForm es ->
        let mfi = case getIdent e of Left _ -> Nothing; Right fi -> Just fi in
        return $ Acc Nothing $ Form ctx mfi es : frames