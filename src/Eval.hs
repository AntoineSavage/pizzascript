{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Eval where

import qualified Ast as A
import qualified Data.Map as M

import Ast ( quote, unquote )
import BuiltIns
import Control.Monad ( forM_, liftM2 )
import Data.Nat ( Nat(..) )
import Types
import Utils

type Result = Maybe (WithPos PzVal)
type EvalResult = Either String Acc

data Acc
    = Acc Result Dict [StackFrame]
    deriving (Show, Eq)

data StackFrame
    = Block [WithPos AstExpr]
    | Form Pos [WithPos AstExpr]
    | Invoc Pos Func [WithPos PzVal] (Maybe [WithPos AstExpr])
    deriving (Show, Eq)

evalMany :: [WithPos AstExpr] -> IO ()
evalMany es = go $ Acc Nothing builtInCtx [Block es]

go :: Acc -> IO ()
go (Acc result ctx []) = return () -- no more frames: halt
go (Acc result ctx (frame:frames)) =
    case evalFrame result ctx frame frames of
        Left s -> putStrLn s
        Right acc -> go acc

evalFrame :: Result -> Dict -> StackFrame -> [StackFrame] -> EvalResult
evalFrame result ctx frame frames =
    case frame of
        Block es -> evalBlock result ctx es frames
        Form p es -> evalForm result ctx p es frames
        Invoc p f as es -> evalInvoc result ctx p f as es frames

evalBlock :: Result -> Dict -> [WithPos AstExpr] -> [StackFrame] -> EvalResult
evalBlock result ctx es frames =
    case es of
        [] ->
            -- block finished: pop frame
            return $ Acc result ctx frames
        
        e:es ->
            -- evaluate next block expression
            evalExpr ctx e Eval $ Block es : frames

evalForm :: Result -> Dict -> Pos -> [WithPos AstExpr] -> [StackFrame] -> EvalResult
evalForm result ctx p elems frames =
    case result of
        Nothing ->
            -- no result to process yet
            case elems of
                [] ->
                    -- empty form -> return unit type (and pop frame)
                    return $ Acc (Just $ WithPos p PzUnit) ctx frames
        
                e:es ->
                    -- evaluate first form element (should be func)
                    evalExpr ctx e Eval $ Form p es : frames
        
        Just f ->
            -- process result (first form elem, should be func)
            case val f of
                PzFunc func ->
                    -- replace form with function invocation
                    return $ Acc Nothing ctx $ Invoc p func [] (Just elems) : frames

                _ -> Left $
                    "Error: Malformed function invocation "
                    ++ "(first form element must be a function)"
                    ++ "\n at: " ++ show p
                    ++ "\n" ++ show f

evalInvoc :: Result -> Dict -> Pos -> Func -> [WithPos PzVal] -> Maybe [WithPos AstExpr] -> [StackFrame] -> EvalResult
evalInvoc result ctx p func as melems frames =
    case result of
        Nothing ->
            -- no result to process yet 
            case melems of
                Nothing -> 
                    -- marked for invocation: invoke function
                    case invokeFunc ctx p func as frames of
                        Left s -> Left $ s ++ "\n at: " ++ show p
                        Right acc -> return acc

                Just [] -> 
                    -- all args evaluated: mark for invocation
                    return $ Acc Nothing ctx $ Invoc p func (reverse as) Nothing : frames

                Just (e:es) ->
                    -- evaluate function argument
                    case evalExpr ctx e (getArgPass func) $ Invoc p func as (Just es) : frames of
                        Left s -> Left $ s ++ "\n at: " ++ show p
                        Right acc -> return acc

        Just r ->
            -- process result
            case melems of
                Nothing ->
                    -- function invocation result (pop frame)
                    -- TODO: Impure functions: handle explicit output context
                    return $ Acc (Just r) ctx frames

                Just es -> 
                    -- argument evaluation result
                    return $ Acc Nothing ctx $ Invoc p func (r:as) (Just es) : frames

evalExpr :: Dict -> WithPos AstExpr -> ArgPass -> [StackFrame] -> EvalResult
evalExpr ctx e@(WithPos p v) eval frames = 
    let setResult result = return $ Acc (Just result) ctx frames
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
        (AstList k elems, Eval) -> return $ Acc Nothing ctx $ Form p (toForm p k elems) : frames

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
                                ++ "\n at: " ++ show p
                                ++ "\n context keys: " ++ show (M.keys m)

                    _ -> Left $
                        "Error: Non-dictionary context for identifier: " ++ show i
                        ++ "\n when evaluating (possibly qualified) identifier: " ++ show ident
                        ++ "\n at: " ++ show p
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
        ["func"] -> returnFrom frames $ _func ctx p args

        -- miscellaneous
        -- TODO

        _ -> Left $ "TODO: Implement built-in function: " ++ show ps

_func :: Dict -> Pos -> [WithPos PzVal] -> FuncReturn
_func ctx p args = do
    es <- mapM (unquote.unevalExpr) args
    fc <- evalFuncCustom es
    return (ctx, WithPos p $ PzFunc $ fromFuncCustom ctx fc)

-- TODO handle explicit context
-- TODO handle args
invokeFuncCustom :: Dict -> Pos -> [WithPos PzVal] -> Dict -> FuncImpureArgs -> FuncArgs -> [WithPos AstExpr] -> [StackFrame] -> EvalResult
invokeFuncCustom ctx p as implCtx impArgs args es frames =
    return $ Acc Nothing implCtx $ Block es : frames

-- Utils
returnFrom :: [StackFrame] -> FuncReturn -> EvalResult
returnFrom frames x = x >>= \(ctx, r) -> return $ Acc (Just r) ctx frames

-- Eval custom function
evalFuncCustom :: [WithPos AstExpr] -> Either String FuncCustom
evalFuncCustom es0 = do
    (impArgs, es1) <- evalImpureArgs es0
    (args, es2) <- evalArgs es1
    return $ FuncCustom impArgs args es2

evalImpureArgs :: [WithPos AstExpr] -> Either String (FuncImpureArgs, [WithPos AstExpr])
evalImpureArgs elems = case elems of
    -- form starting with argument-passing behaviour symbol, followed by...
    WithPos p (AstList KindForm (WithPos p2 (AstSymb s@(Symb Z (Ident [_]))):xs)):es -> do
        argPass <- case symbToArgPass s of
            Just r -> return $ WithPos p2 r
            Nothing -> Left $
                "Error: Invalid argument-passing behaviour symbol: " ++ show s
                ++ "\n at: " ++ show p

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
    let toIdent e = case val e of
            AstIdent ident@(Ident [i]) -> return $ fmap (const ident) e
            _ -> Left $
                "Error: Function arity argument must be an unqualified identifier: " ++ show (val e)
                ++ "\n at: " ++ show (pos e)

    in case elems of
        WithPos p (AstIdent ident@(Ident [i])):es -> return (ArgsVaria (WithPos p ident), es)
        WithPos p (AstList KindForm ies):es -> (,es) . ArgsArity <$> mapM toIdent ies
        _ -> Left $
            "Error: Function arguments must be either:"
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
unevalImpureArgs impureArgs =
    let toExpr = fmap $ AstSymb .argPassToSymb
        toForm p = WithPos p . AstList KindForm
    in case impureArgs of
        None -> []
        ArgPass p ap -> [ toForm p [toExpr ap] ]
        Both p ap ec -> [ toForm p [toExpr ap, fmap (AstSymb .symb) ec] ]

unevalArgs :: FuncArgs -> [WithPos AstExpr]
unevalArgs args =
    case args of
        ArgsVaria ident -> [fmap AstIdent ident]
        ArgsArity is -> map (fmap AstIdent) is