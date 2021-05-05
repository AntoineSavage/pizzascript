module Main where

import qualified Ast as A
import qualified Data.Map as M

import Ast ( parseExpr, parseMany, ignore )
import BuiltIns ( FuncReturn, builtInCtx, _not, _or, _and )
import Control.Monad ( forM_, liftM2 )
import Data.Maybe ( fromMaybe )
import Data.Nat ( Nat(..) )
import Eval
import Quote ( quote, unquote )
import Text.Parsec ( eof )
import Text.Parsec.String ( parseFromFile )
import Types
import Utils

main :: IO ()
main = do
    let parser = parseMany ignore parse eof
        parse = parseExpr ignore parse
    mes <- parseFromFile parser "example/small.pz"
    case mes of
        Left err -> print err
        Right es -> evalMany es

evalMany :: [WithPos AstExpr] -> IO ()
evalMany es = go $ Acc Nothing [Block builtInCtx es]

go :: Acc -> IO ()
go (Acc result []) = putStrLn "Halting" -- no more frames: halt
go (Acc result (frame:frames)) = do
    putStrLn "=========="
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Frame: TODO"
    putStrLn $ "Nbr additional frames: " ++ show (length frames)
    putStrLn "----------"
    case evalFrame result frame frames of
        Left s -> putStrLn s
        Right acc -> go acc

evalFrame :: Result -> StackFrame -> [StackFrame] -> EvalResult
evalFrame result frame frames =
    case frame of
        Block ctx es -> evalBlock result ctx es frames
        Form ctx p mfi es -> evalForm result ctx p mfi es frames
        Invoc ctx p fi f as es -> evalInvoc result ctx p fi f as es frames

evalBlock :: Result -> Dict -> [WithPos AstExpr] -> [StackFrame] -> EvalResult
evalBlock result ctx es frames =
    case es of
        [] ->
            -- block finished: pop frame
            return $ Acc result frames
        
        e:es ->
            -- evaluate next block expression
            evalExpr ctx e Eval $ Block ctx es : frames

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
                    -- evaluate first form element (should be func)
                    evalExpr ctx e Eval $ Form ctx p (getIdent e) es : frames
        
        Just f ->
            -- process result (first form elem, should be func)
            case val f of
                PzFunc func ->

                    -- handle 'func if needed (reduces number of built-in dependencies)
                    if func == func
                        then case invokeFuncSpecial ctx p elems frames of
                            Left s -> Left $ addIdentAndPos p mfi s
                            Right acc -> return acc

                        -- replace form with function invocation
                        else  return $ Acc Nothing $ Invoc ctx p mfi func [] (Just elems) : frames

                _ -> Left $
                    "Error: Malformed function invocation (first form element must be a function)"
                    ++ fromMaybe "" (flip fmap mfi $ \fi -> "\n when invoking function: " ++ show fi)
                    ++ "\n at: " ++ show p
                    ++ "\n" ++ show f

evalInvoc :: Result -> Dict -> Pos -> Maybe (WithPos Ident) -> Func -> [WithPos PzVal] -> Maybe [WithPos AstExpr] -> [StackFrame] -> EvalResult
evalInvoc result ctx p mfi func as melems frames =
    case result of
        Nothing ->
            -- no result to process yet 
            case melems of
                Nothing -> 
                    -- marked for invocation: invoke function
                    case invokeFunc ctx p func as frames of
                        Left s -> Left $ addIdentAndPos p mfi s
                        Right acc -> return acc

                Just [] -> 
                    -- all args evaluated: mark for invocation
                    return $ Acc Nothing $ Invoc ctx p mfi func (reverse as) Nothing : frames

                Just (e:es) ->
                    -- evaluate function argument according to argument-passing behaviour
                    case evalExpr ctx e (getArgPass func) $ Invoc ctx p mfi func as (Just es) : frames of
                        Left s -> Left $ addIdentAndPos p mfi s
                        Right acc -> return acc

        Just r ->
            -- process result
            case melems of
                Just es -> 
                    -- argument evaluation result
                    return $ Acc Nothing $ Invoc ctx p mfi func (r:as) (Just es) : frames

                _ ->
                    -- function invocation result (pop frame)
                    case impArgs func of
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

invokeFunc :: Dict -> Pos -> Func -> [WithPos PzVal] -> [StackFrame] -> EvalResult
invokeFunc ctx p (Func implCtx impArgs args body) as frames =
    case body of
        BodyBuiltIn ident -> invokeFuncBuiltIn ctx p as ident frames
        BodyCustom es -> invokeFuncCustom ctx p as implCtx impArgs args es frames

returnFrom :: [StackFrame] -> FuncReturn -> EvalResult
returnFrom frames x = x >>= \(ctx, r) -> return $ Acc (Just r) $ setCtx ctx frames

setCtx :: Dict -> [StackFrame] -> [StackFrame]
setCtx ctx frames = case frames of
    [] -> []
    (frame:fs) -> (:fs) $ case frame of
        Block _ es -> Block ctx es
        Form _ p mfi es-> Form ctx p mfi es
        Invoc _ p mfi f as es -> Invoc ctx p mfi f as es

invokeFuncSpecial :: Dict -> Pos -> [WithPos AstExpr] -> [StackFrame] -> EvalResult
invokeFuncSpecial ctx p es frames = do
    fc <- evalFuncCustom es
    let r = WithPos p $ PzFunc $ fromFuncCustom ctx fc
    return $ Acc (Just r) frames

invokeFuncCustom :: Dict -> Pos -> [WithPos PzVal] -> Dict -> FuncImpureArgs -> FuncArgs -> [WithPos AstExpr] -> [StackFrame] -> EvalResult
invokeFuncCustom explCtx p as implCtx impArgs args es frames =
    let toExpr = fmap $ PzSymb .symb
        explCtxPairs = case impArgs of
            Both _ _ i -> [ (toExpr i, WithPos (pos i) $ PzDict explCtx) ]
            _ -> []
        
        actLen = length as
        (expLen, argPairs) = case args of
            ArgsVaria i -> (actLen, [ (toExpr i, WithPos (pos i) $ PzList as) ])
            ArgsArity is -> (length is, zip (map toExpr is) as)

        pairs = explCtxPairs ++ argPairs
        f acc (k, v) = M.insert k v acc
        finalImplCtx = foldl f implCtx pairs

    in if actLen == expLen
            then return $ Acc Nothing $ Block finalImplCtx es : frames
            else Left $ 
                "Error: Invoking function with incorrect number of arguments:"
                ++ "\n expected: " ++ show expLen
                ++ "\n received: " ++ show actLen

-- Built-in functions
invokeFuncBuiltIn :: Dict -> Pos -> [WithPos PzVal] -> Ident -> [StackFrame] -> EvalResult
invokeFuncBuiltIn ctx p args (Ident ps) frames =
    case ps of
        -- numbers
        -- TODO

        -- strings
        -- TODO

        -- symbols
        -- TODO

        -- booleans
        ["not"] -> returnFrom frames $ f1 args $ \x -> fpure ctx $ _not x
        ["or"] -> returnFrom frames $ f2 args $ \x y -> fpure ctx $ _or x y
        ["and"] -> returnFrom frames $ f2 args $ \x y -> fpure ctx $ _and x y

        -- lists
        -- TODO

        -- dictionaries
        -- TODO

        -- functions
        -- TODO

        -- miscellaneous
        -- TODO

        _ -> Left $ "TODO: Implement built-in function: " ++ show ps