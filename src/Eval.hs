{-# LANGUAGE TupleSections #-}
module Eval where

import qualified Ast as A
import qualified Data.Map as M

import Ast ( quote, unquote )
import BuiltIns
import Control.Monad ( forM_, liftM2 )
import Data.Nat ( Nat(..) )
import Types
import Utils ( symb, toForm )

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

                _ -> Left $ "Error: Malformed function invocation "
                    ++ "(first form element must be a function): "
                    ++ show f
                    ++ "\n at: " ++ show p

evalInvoc :: Result -> Dict -> Pos -> Func -> [WithPos PzVal] -> Maybe [WithPos AstExpr] -> [StackFrame] -> EvalResult
evalInvoc result ctx p func as melems frames =
    case result of
        Nothing ->
            -- no result to process yet 
            case melems of
                Nothing -> 
                    -- marked for invocation: invoke function
                    case invokeFunc ctx p func (reverse as) frames of
                        Left s -> Left $ s ++ "\n at: " ++ show p
                        Right acc -> return acc

                Just [] -> 
                    -- all args evaluated: mark for invocation
                    return $ Acc Nothing ctx $ Invoc p func as Nothing : frames

                Just (e:es) ->
                    -- evaluate function argument
                    case evalExpr ctx e (argPass func) $ Invoc p func as (Just es) : frames of
                        Left s -> Left $ s ++ "\n at: " ++ show p
                        Right acc -> return acc

        Just r ->
            -- process result
            case melems of
                Nothing ->
                    -- function invocation result (pop frame)
                    return $ Acc (Just r) ctx frames

                Just es -> 
                    -- argument evaluation result
                    return $ Acc Nothing ctx $ Invoc p func (r:as) (Just es) : frames

evalExpr :: Dict -> WithPos AstExpr -> FuncArgPass -> [StackFrame] -> EvalResult
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
            [] -> Right val_or_ctx
            s@(Symb _ i):ss ->
                case val val_or_ctx of
                    PzDict m ->
                        case M.lookup (withPos $ PzSymb s) m of
                            Just v' -> inner v' ss

                            Nothing -> Left $  "Error: Undefined identifier: " ++ show i
                                ++ "\n when evaluating (possibly qualified) identifier: " ++ show ident
                                ++ "\n at: " ++ show p
                                -- TODO Display only single-quoted, unqualified symbols, as identifiers
                                ++ "\n context keys: " ++ show (M.keys m)

                    _ -> Left $  "Error: Evaluating child identifier in non-dictionary context"
                                ++ "\n child identifier: " ++ show i
                                ++ "\n part of qualified identifier: " ++ show ident
                                ++ "\n at: " ++ show p
                                ++ "\n non-dictionary context: " ++ show val_or_ctx

-- TODO handle implicit context
-- TODO handle explicit context
-- TODO handle args
invokeFunc :: Dict -> Pos -> Func -> [WithPos PzVal] -> [StackFrame] -> EvalResult
invokeFunc ctx p func args frames =
    case body func of
        BodyBuiltIn ident -> invokeFuncBuiltIn ctx p args ident frames
        BodyCustom es -> Left $ "TODO: Invoke custom function: " ++ show es

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
        ["func"] -> do
            es <- mapM (unquote.unevalExpr) args
            returnFrom frames $ (ctx,) . WithPos p . PzFunc . fromFuncCustom ctx <$> evalFuncCustom es

        -- miscellaneous
        -- TODO

        _ -> Left $ "TODO: Implement built-in function: " ++ show ps

returnFrom :: [StackFrame] -> FuncReturn -> EvalResult
returnFrom frames x = x >>= \(ctx, r) -> return $ Acc (Just r) ctx frames

-- Uneval
unevalExpr :: WithPos PzVal -> WithPos AstExpr
unevalExpr (WithPos p v) = WithPos p $
    case v of
        PzUnit -> AstList KindForm []
        PzNum n -> AstNum n
        PzStr s -> AstStr s
        PzSymb s -> AstSymb s
        PzList l -> AstList KindList $ map unevalExpr l
        PzDict m -> AstList KindDict $ map (\(k, v) -> withPos $ AstList KindForm [unevalExpr k, unevalExpr v]) $ M.assocs m
        PzFunc f ->
            case toFuncCustom f of
                Left ident -> AstSymb $ symb ident
                Right fc -> AstList KindForm $ unevalFuncCustom fc

-- Eval / uneval func custom
data FuncCustom
    = FuncCustom FuncExplCtx FuncArgPass FuncArgs [WithPos AstExpr]
    deriving (Show, Eq)

toFuncCustom :: Func -> Either Ident FuncCustom
toFuncCustom func =
    case body func of
        BodyBuiltIn ident -> Left ident
        BodyCustom es -> Right $ FuncCustom (explCtx func) (argPass func) (args func) es

fromFuncCustom :: Dict -> FuncCustom -> Func
fromFuncCustom ctx (FuncCustom explCtx argPass args es) =
    Func ctx explCtx argPass args $ BodyCustom es

evalFuncCustom :: [WithPos AstExpr] -> Either String FuncCustom
evalFuncCustom = undefined -- TODO

unevalFuncCustom :: FuncCustom -> [WithPos AstExpr]
unevalFuncCustom = undefined -- TODO