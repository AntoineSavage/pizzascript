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

type Result = Maybe PzVal
type EvalResult = Either String Acc

data Acc
    = Acc Result Dict [StackFrame]
    deriving (Show, Eq)

data StackFrame
    = Block [AstExpr]
    | Form Pos [AstExpr]
    | Invoc Pos Func [PzVal] (Maybe [AstExpr])
    deriving (Show, Eq)

eval :: [AstExpr] -> IO ()
eval es = go $ Acc Nothing builtInCtx [Block es]

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

evalBlock :: Result -> Dict -> [AstExpr] -> [StackFrame] -> EvalResult
evalBlock result ctx es frames =
    case es of
        [] ->
            -- block finished: pop frame
            return $ Acc result ctx frames
        
        e:es ->
            -- evaluate next block expression
            evalExpr ctx e Eval $ Block es : frames

evalForm :: Result -> Dict -> Pos -> [AstExpr] -> [StackFrame] -> EvalResult
evalForm result ctx p elems frames =
    case result of
        Nothing ->
            -- no result to process yet
            case elems of
                [] ->
                    -- empty form -> return unit type (and pop frame)
                    return $ Acc (Just PzUnit) ctx frames
        
                e:es ->
                    -- evaluate first form element (should be func)
                    evalExpr ctx e Eval $ Form p es : frames
        
        Just f ->
            -- process result (first form elem, should be func)
            case f of
                PzFunc func ->
                    -- replace form with function invocation
                    return $ Acc Nothing ctx $ Invoc p func [] (Just elems) : frames

                _ -> Left $ "Error: Malformed function invocation "
                    ++ "(first form element must be a function): "
                    ++ show f
                    ++ "\n at: " ++ show p

evalInvoc :: Result -> Dict -> Pos -> Func -> [PzVal] -> Maybe [AstExpr] -> [StackFrame] -> EvalResult
evalInvoc result ctx p func as melems frames =
    case result of
        Nothing ->
            -- no result to process yet 
            case melems of
                Nothing -> 
                    -- marked for invocation: invoke function
                    case invokeFunc ctx func (reverse as) frames of
                        Left s -> Left $ s ++ "\n at: " ++ show p
                        Right acc -> return acc

                Just [] -> 
                    -- all args evaluated: mark for invocation
                    return $ Acc Nothing ctx $ Invoc p func as Nothing : frames

                Just (e:es) ->
                    -- evaluate function argument
                    let Func _ _ argPass _ _ = func in
                    case evalExpr ctx e argPass $ Invoc p func as (Just es) : frames of
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

evalExpr :: Dict -> AstExpr -> FuncArgPass -> [StackFrame] -> EvalResult
evalExpr ctx e@(AstExpr p v) eval frames = 
    let setResult result = return $ Acc (Just result) ctx frames in
    case (v, eval) of
        -- numbers and strings
        (AstNum n, _) -> setResult $ PzNum n
        (AstStr s, _) -> setResult $ PzStr s

        -- symbols
        (AstSymb symb, Eval) -> setResult $ PzSymb symb

        -- identifiers
        (AstIdent ident, Eval) -> evalIdent ctx p ident >>= setResult
        (AstIdent ident, DeepQuote) -> evalIdent ctx p ident >>= \v -> evalExpr ctx (uneval v) Quote frames
        (AstIdent ident, DeepUnquote) -> evalIdent ctx p ident >>= \v -> evalExpr ctx (uneval v) Unquote frames

        -- lists
        (AstList k elems, Eval) -> return $ Acc Nothing ctx $ Form p (toForm p k elems) : frames

        -- quote and unquote
        (_, Quote) -> evalExpr ctx (quote e) Eval frames
        (_, Unquote) -> unquote e >>= \e' -> evalExpr ctx e' Eval frames
        (_, DeepQuote) -> evalExpr ctx e Quote frames
        (_, DeepUnquote) -> evalExpr ctx e Unquote frames

evalIdent :: Dict -> Pos -> Ident -> Either String PzVal
evalIdent ctx p ident = inner (PzDict ctx) $ symbSplitImpl $ symb ident where
    inner val symbs =
        case symbs of
            [] -> Right val
            s:ss ->
                case val of
                    PzDict m ->
                        case M.lookup (PzSymb s) m of
                            Just v' -> inner v' ss
                            Nothing -> Left $  "Error: Undefined identifier: " ++ show ident
                                ++ "\n at: " ++ show p
                                ++ "\n ctx keys: " ++ show (M.keys ctx)
                    _ -> Left $  "Error: Non-dictionary field request: " ++ show s
                                ++ "\n got: " ++ show val
                                ++ "\n for qualified identifier: " ++ show ident
                                ++ "\n at: " ++ show p

-- TODO handle definition context
-- TODO handle impure context ident
-- TODO handle arg idents
invokeFunc :: Dict -> Func -> [PzVal] -> [StackFrame] -> EvalResult
invokeFunc ctx (Func _ _ _ _ body) args frames =
    case body of
        BodyBuiltIn ident -> invokeFuncBuiltIn ctx args ident frames
        BodyCustom es -> Left $ "TODO: Invoke custom function: " ++ show es

invokeFuncBuiltIn :: Dict -> [PzVal] -> Ident -> [StackFrame] -> EvalResult
invokeFuncBuiltIn ctx args (Ident ps) frames =
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
            es <- mapM (unquote.uneval) args
            returnFrom frames $ (ctx,) . PzFunc <$> evalFunc ctx es

        -- miscellaneous
        -- TODO

        _ -> Left $ "TODO: Implement built-in function: " ++ show ps

returnFrom :: [StackFrame] -> FuncReturn -> EvalResult
returnFrom frames x = x >>= \(ctx, r) -> return $ Acc (Just r) ctx frames

-- Uneval
uneval :: PzVal -> AstExpr
uneval v = AstExpr undefined $
    case v of
        PzUnit -> AstList KindForm []
        PzNum n -> AstNum n
        PzStr s -> AstStr s
        PzSymb s -> AstSymb s
        PzList l -> AstList KindList $ map uneval l
        PzDict m -> AstList KindDict $ map (\(k, v) -> uneval $ PzList [k, v]) $ M.assocs m
        PzFunc f -> AstList KindForm $ unevalFunc f

-- Func eval / uneval
evalFunc :: Dict -> [AstExpr] -> Either String Func
evalFunc = undefined -- TODO

unevalFunc :: Func -> [AstExpr]
unevalFunc = undefined -- TODO

-- TODO: FuncCustom: FuncExplCtx FuncArgPass FuncArgs [AstExpr]
-- toFuncCustom :: Func -> Either Ident FuncCustom (returns: Left BodyBuiltIn.ident)
-- fromFuncCustom :: Dict -> FuncCustom -> Func