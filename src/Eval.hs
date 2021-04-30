module Eval where

import qualified Ast as A
import qualified Data.Map as M

import BuiltIns
import Control.Monad ( forM_, liftM2 )
import Data.Maybe ( fromMaybe )
import Data.Nat ( Nat(..) )
import Types

data Acc
    = Acc (Maybe PzVal) Dict [StackFrame]
    deriving (Show, Eq)

data StackFrame
    = Block [AstExpr]
    | Form AstPos [AstExpr]
    | Invoc AstPos Func [PzVal] [AstExpr]
    deriving (Show, Eq)

evalAst :: Ast -> IO ()
evalAst (Ast _ es) = go $ Acc Nothing M.empty [Block es]

go :: Acc -> IO ()
go (Acc result ctx []) = return () -- halt
go (Acc result ctx (frame:frames)) =
    case frame of
        Block [] ->
            -- block finished: pop stack frame
            go $ Acc result ctx frames
        
        Block (e:es) ->
            -- evaluate expression
            case evalExpr ctx e $ Block es : frames of
                Left s -> putStrLn s
                Right acc -> go acc

        Form p es ->
            -- evaluate form
            case evalForm result ctx p es frames of
                Left s -> putStrLn s
                Right acc -> go acc
        
        Invoc p f as es ->
            -- evaluate function invocation
            case evalInvoc result ctx p f as es frames of
                Left s -> putStrLn s
                Right acc -> go acc

evalExpr :: Dict -> AstExpr -> [StackFrame] -> Either String Acc
evalExpr ctx (AstExpr p _ v) frames = 
    let setResult result = return $ Acc (Just result) ctx frames in
    case v of
        -- nums, strs and symbs return themselves
        AstNum n -> setResult $ PzNum n
        AstStr s -> setResult $ PzStr s
        AstSymb symb -> setResult $  PzSymb symb

        -- identifiers return the corresponding value in ctx
        AstIdent ident ->
            case dictGet (PzSymb $ fromIdent ident) ctx of
                PzUnit -> Left $ "Error: Undefined identifier: " ++ show ident ++ "\n at: " ++ show p
                val -> setResult val

        -- lists push form on stack
        AstList k _ elems -> return $ Acc Nothing ctx $ Form p (toForm p k elems) : frames

evalForm :: Maybe PzVal -> Dict -> AstPos -> [AstExpr] -> [StackFrame] -> Either String Acc
evalForm result ctx p elems frames =
    case result of
        Nothing ->
            case elems of
                [] ->
                    -- empty form -> return unit type
                    return $ Acc (Just PzUnit) ctx frames
        
                e:es ->
                    -- evaluate first form element (should be func)
                    evalExpr ctx e $ Form p es : frames
        
        Just f ->
            -- first form element evaluated (should be func)
            -- replace with invocation
            case f of
                PzFunc func -> return $ Acc Nothing ctx $ Invoc p func [] elems : frames
                _ -> Left $ "Error: Malformed function invocation (first form element must be a function): " ++ show f ++ "\n at: " ++ show p

evalInvoc :: Maybe PzVal -> Dict -> AstPos -> Func -> [PzVal] -> [AstExpr] -> [StackFrame] -> Either String Acc
evalInvoc result ctx p f as es frames =
    case result of
        Nothing -> 
            case es of
                [] -> 
                    -- all args evaluated: invoke function
                    -- TODO take impure context symbol into account
                    -- TODO take arg symbols into account
                    Left "TODO: EvalInvoc"

                (e:es) ->
                    -- evaluate function argument
                    -- TODO take ArgPass into account
                    case evalExpr ctx e $ Invoc p f as es : frames of
                        Left s -> Left $ s ++ "\n at: " ++ show p
                        Right acc -> return acc
        
        Just f ->
            -- invocation finished: pop stack frame
            return $ Acc result ctx frames

-- 
-- invokeFunc :: Ident -> [AstExpr] -> IO PzVal
-- invokeFunc (Ident ps) args =
--     case ps of
--         ["list"] -> mapM evalExpr args >>= list
--         ["dict"] -> mapM evalDictEntry args >>= dict
--         ["func"] -> return PzUnit
--         _ -> return PzUnit -- Error: undefined identifier
-- 
-- evalDictEntry :: AstExpr -> IO (PzVal, PzVal)
-- evalDictEntry (AstExpr _ _ v) =
--     case v of
--         (AstList KindForm _ [k, v]) -> liftM2 (,) (evalExpr k) (evalExpr v)
-- 
--         _ -> return (PzUnit, PzUnit) -- Error: malformed dictionary entry

-- list :: [PzVal] -> PzVal
-- list = PzList
-- 
-- dict :: Dict -> PzVal
-- dict = PzDict

dictGet :: PzVal -> Dict -> PzVal
dictGet k m = fromMaybe PzUnit $ M.lookup k m