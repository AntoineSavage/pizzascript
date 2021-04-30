module Eval where

import qualified Ast as A
import qualified Data.Map as M

import BuiltIns
import Control.Monad ( forM_, liftM2 )
import Data.Nat ( Nat(..) )
import Types
import Utils ( dictGet, symb, toForm, FuncReturn )

type Result = Maybe PzVal
type EvalResult = Either String Acc

data Acc
    = Acc Result Dict [StackFrame]
    deriving (Show, Eq)

data StackFrame
    = Block [AstExpr]
    | Form AstPos [AstExpr]
    | Invoc AstPos Func [PzVal] (Maybe [AstExpr])
    deriving (Show, Eq)

evalAst :: Ast -> IO ()
evalAst (Ast _ es) = go $ Acc Nothing builtInCtx [Block es]

go :: Acc -> IO ()
go (Acc result ctx []) = return () -- no more frames: halt
go (Acc result ctx (frame:frames)) = do
    let macc = evalFrame result ctx frame frames
    case macc of
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
            -- evaluate expression
            evalExpr ctx e $ Block es : frames

evalExpr :: Dict -> AstExpr -> [StackFrame] -> EvalResult
evalExpr ctx (AstExpr p _ v) frames = 
    let setResult result = return $ Acc (Just result) ctx frames in
    case v of
        -- nums, strs and symbs return themselves
        AstNum n -> setResult $ PzNum n
        AstStr s -> setResult $ PzStr s
        AstSymb symb -> setResult $  PzSymb symb

        -- identifiers return the corresponding value in ctx
        AstIdent ident ->
            case dictGet (PzSymb $ symb ident) ctx of
                PzUnit -> Left $ "Error: Undefined identifier: " ++ show ident
                    ++ "\n at: " ++ show p
                    ++ "\n ctx keys: " ++ show (M.keys ctx)
                val -> setResult val

        -- lists push form on stack
        AstList k _ elems -> return $ Acc Nothing ctx $ Form p (toForm p k elems) : frames

evalForm :: Result -> Dict -> AstPos -> [AstExpr] -> [StackFrame] -> EvalResult
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
                    evalExpr ctx e $ Form p es : frames
        
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

evalInvoc :: Result -> Dict -> AstPos -> Func -> [PzVal] -> Maybe [AstExpr] -> [StackFrame] -> EvalResult
evalInvoc result ctx p func as melems frames =
    case result of
        Nothing ->
            -- no result to process yet 
            case melems of
                Nothing -> 
                    -- marked for invocation: invoke function
                    case invokeFunc ctx func as frames of
                        Left s -> Left $ s ++ "\n at: " ++ show p
                        Right acc -> return acc

                Just [] -> 
                    -- all args evaluated: mark for invocation
                    return $ Acc result ctx $ Invoc p func as Nothing : frames

                Just (e:es) ->
                    -- evaluate function argument
                    -- TODO take ArgPass into account
                    case evalExpr ctx e $ Invoc p func as (Just es) : frames of
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

-- TODO take impure context symbol into account
-- TODO take arg symbols into account
invokeFunc :: Dict -> Func -> [PzVal] -> [StackFrame] -> EvalResult
invokeFunc ctx (Func _ _ _ body) args frames =
    case body of
        BodyBuiltIn name -> invokeFuncBuiltIn ctx args name frames
        BodyCustom es -> Left $ "TODO: Invoke custom function: " ++ show es

invokeFuncBuiltIn :: Dict -> [PzVal] -> String -> [StackFrame] -> EvalResult
invokeFuncBuiltIn ctx args name frames =
    case name of
        "_not" -> returnFrom frames $ _not ctx args
        "_or" -> returnFrom frames $ _or ctx args
        "_and" -> returnFrom frames $ _and ctx args
        _ -> Left $ "TODO: Invoke built-in function: " ++ name

returnFrom :: [StackFrame] -> FuncReturn -> EvalResult
returnFrom frames x =
    case x of
        Left s -> Left s
        Right (ctx, r) -> Right $ Acc (Just r) ctx frames