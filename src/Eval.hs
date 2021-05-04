{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Eval where

import qualified Ast as A
import qualified Data.Map as M

import Ast ( quote, unquote )
import BuiltIns
import Control.Monad ( forM_, liftM2 )
import Data.Maybe
import Data.Nat ( Nat(..) )
import Pretty
import Types
import Utils

type EvalResult = Either String Acc

evalMany :: [WithPos AstExpr] -> IO ()
evalMany es = go $ Acc Nothing [Block builtInCtx es]

go :: Acc -> IO ()
go (Acc result []) = putStrLn "Halting" -- no more frames: halt
go (Acc result (frame:frames)) = do
    putStrLn "=========="
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Frame: " ++ prettyFrame 0 frame
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

evalExpr :: Dict -> WithPos AstExpr -> ArgPass -> [StackFrame] -> EvalResult
evalExpr ctx e@(WithPos p v) eval frames = 
    let setResult result = return $ Acc (Just result) frames
        setResult' = setResult . WithPos p
    in
    case (v, eval) of
        -- numbers and strings
        (AstNum n, _) -> setResult' $ PzNum n
        (AstStr s, _) -> setResult' $ PzStr s

        -- symbols
        (AstSymb symb, Eval) -> setResult' $ PzSymb symb

        -- identifiers
        (AstIdent ident, Eval) -> evalIdent ctx p ident >>= setResult
        (AstIdent ident, DeepQuote) -> evalIdent ctx p ident >>= \r -> evalExpr ctx (unevalExpr r) Quote frames
        (AstIdent ident, DeepUnquote) -> evalIdent ctx p ident >>= \r -> evalExpr ctx (unevalExpr r) Unquote frames

        -- lists
        (AstList k elems, Eval) -> return $ Acc Nothing $ Form ctx p Nothing (toForm p k elems) : frames

        -- quote and unquote
        (_, Quote) -> evalExpr ctx (quote e) Eval frames
        (_, Unquote) -> unquote e >>= \e' -> evalExpr ctx e' Eval frames
        (_, DeepQuote) -> evalExpr ctx e Quote frames
        (_, DeepUnquote) -> evalExpr ctx e Unquote frames

evalIdent :: Dict -> Pos -> Ident -> Either String (WithPos PzVal)
evalIdent ctx p ident = inner (withPos $ PzDict ctx) $ symbSplitImpl $ symb ident where
    inner val_or_ctx symbs =
        case symbs of
            [] -> return val_or_ctx
            s@(Symb _ i):ss ->
                case val val_or_ctx of
                    PzDict m ->
                        case M.lookup (withPos $ PzSymb s) m of
                            Just v' -> inner v' ss

                            Nothing -> Left $
                                "Error: Undefined identifier: " ++ show i
                                ++ "\n when evaluating (possibly qualified) identifier: " ++ show ident
                                ++ "\n context keys: " ++ show (M.keys m)

                    _ -> Left $
                        "Error: Non-dictionary context for identifier: " ++ show i
                        ++ "\n when evaluating (possibly qualified) identifier: " ++ show ident
                        ++ "\n non-dictionary context: " ++ show val_or_ctx

invokeFunc :: Dict -> Pos -> Func -> [WithPos PzVal] -> [StackFrame] -> EvalResult
invokeFunc ctx p (Func implCtx impArgs args body) as frames =
    case body of
        BodyBuiltIn ident -> invokeFuncBuiltIn ctx p as ident frames
        BodyCustom es -> invokeFuncCustom ctx p as implCtx impArgs args es frames

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
        ["not"] -> returnFrom frames $ _not ctx args
        ["or"] -> returnFrom frames $ _or ctx args
        ["and"] -> returnFrom frames $ _and ctx args

        -- lists
        -- TODO

        -- dictionaries
        -- TODO

        -- functions
        -- TODO

        -- miscellaneous
        -- TODO

        _ -> Left $ "TODO: Implement built-in function: " ++ show ps

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

-- Eval custom function
evalFuncCustom :: [WithPos AstExpr] -> Either String FuncCustom
evalFuncCustom es0 = do
    (impArgs, es1) <- evalImpureArgs es0
    (args, es2) <- evalArgs es1
    validateNoDuplicateIdents impArgs args
    return $ FuncCustom impArgs args es2

validateNoDuplicateIdents :: FuncImpureArgs -> FuncArgs -> Either String ()
validateNoDuplicateIdents impArgs args =
    let explCtxIdents = case impArgs of
            Both _ _ i -> [i]
            _ -> []
        
        argIdents = case args of
            ArgsVaria i -> [i]
            ArgsArity is -> is
    
        duplicates = getDuplicates $ explCtxIdents ++ argIdents
    in if null duplicates
        then return ()
        else Left $
            "Error: Duplicate identifiers in function definition: " ++ show duplicates

evalImpureArgs :: [WithPos AstExpr] -> Either String (FuncImpureArgs, [WithPos AstExpr])
evalImpureArgs elems = case elems of
    -- form starting with argument-passing behaviour symbol, followed by...
    WithPos p (AstList KindForm (WithPos p2 (AstSymb s@(Symb Z (Ident [_]))):xs)):es -> do
        argPass <- case symbToArgPass s of
            Just r -> return $ WithPos p2 r
            Nothing -> Left $
                "Error: Invalid argument-passing behaviour symbol: " ++ show s
                ++ "\n at: " ++ show p2

        case xs of
            -- nothing
            [] -> return (ArgPass p argPass, es)

            -- unqualified identifier
            [ WithPos p3 (AstIdent ec@(Ident [_]))
                ] -> return (Both p argPass $ WithPos p3 ec,es)

    -- no match: assume no impure args
            _ -> return (None, elems)
    _ -> return (None, elems)

evalArgs :: [WithPos AstExpr] -> Either String (FuncArgs, [WithPos AstExpr])
evalArgs elems =
    let toIdent e = case val <$> getIdent e of
            Just ident@(Ident [i]) -> return $ WithPos (pos e) ident
            _ -> Left $
                "Error: Function arity argument must be an unqualified identifier: " ++ show (val e)
                ++ "\n at: " ++ show (pos e)

    in case elems of
        WithPos p (AstIdent ident@(Ident [i])):es -> return (ArgsVaria (WithPos p ident), es)
        WithPos p (AstList KindForm ies):es -> (,es) . ArgsArity <$> mapM toIdent ies
        _ -> Left $
            "Error: Function argument definition must be either:"
            ++ "\n - a single varargs unqualified identifier"
            ++ "\n - an form of arity unqualified identifiers"
            ++ "\n was: " ++ show (map val elems)

-- Uneval
unevalExpr :: WithPos PzVal -> WithPos AstExpr
unevalExpr val = flip fmap val $ \case
    PzUnit -> AstList KindForm []
    PzNum n -> AstNum n
    PzStr s -> AstStr s
    PzSymb s -> AstSymb s
    PzList l -> AstList KindList $ map unevalExpr l
    PzDict m -> AstList KindDict $ flip map (M.assocs m) $
        \(k, v) -> withPos $ AstList KindForm [unevalExpr k, unevalExpr v]
    PzFunc f ->
        case toFuncCustom f of
            Left ident -> AstSymb $ symb ident
            Right fc -> AstList KindForm $ unevalFuncCustom fc

unevalFuncCustom :: FuncCustom -> [WithPos AstExpr]
unevalFuncCustom (FuncCustom impArgs args body) = unevalImpureArgs impArgs ++ unevalArgs args ++ body

unevalImpureArgs :: FuncImpureArgs -> [WithPos AstExpr]
unevalImpureArgs impArgs =
    let toExpr = fmap $ AstSymb .argPassToSymb
        toForm p = WithPos p . AstList KindForm
    in case impArgs of
        None -> []
        ArgPass p ap -> [ toForm p [toExpr ap] ]
        Both p ap ec -> [ toForm p [toExpr ap, fmap (AstSymb .symb) ec] ]

unevalArgs :: FuncArgs -> [WithPos AstExpr]
unevalArgs args =
    case args of
        ArgsVaria ident -> [fmap AstIdent ident]
        ArgsArity is -> map (fmap AstIdent) is