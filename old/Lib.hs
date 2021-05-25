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


-- Eval
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Eval where

import qualified Data.Map as M

import Control.Monad ( forM_, liftM2 )
import Data.AstExpr ( AstExpr(..) )
import Data.Func.ArgPass ( ArgPass(..), argPassToSymb, symbToArgPass )
import Data.Func.FuncArgs ( FuncArgs(..) )
import Data.Func.FuncCustom ( FuncCustom(..), toFuncCustom )
import Data.Func.FuncImpureArgs ( FuncImpureArgs(..) )
import Data.Ident ( Ident )
import Data.Lst ( Lst(..), LstKind(..) )
import Data.Nat ( Nat(Z) )
import Data.PzVal ( Dict, PzVal(..) )
import Data.Symb ( Symb(Symb), symb )
import Quote ( quote, unquote )
import Utils ( Result, getDuplicates, getIdent, toForm )

data ExprEvalResult
    = Evaled PzVal
    | ExprForm [AstExpr]
    deriving (Show, Eq)

evalExpr :: Dict -> AstExpr -> ArgPass -> Result ExprEvalResult
evalExpr ctx e eval =
    let evaled = return . Evaled in
    case (e, eval) of
        -- numbers and strings
        (AstNum n, _) -> evaled $ PzNum n
        (AstStr s, _) -> evaled $ PzStr s

        -- symbols
        (AstSymb symb, Eval) -> evaled $ PzSymb symb

        -- identifiers
        (AstIdent ident, Eval) -> Evaled <$> evalIdent ctx ident
        (AstIdent ident, DeepQuote) -> evalIdent ctx ident >>= \r -> evalExpr ctx (unevalExpr r) Quote
        (AstIdent ident, DeepUnquote) -> evalIdent ctx ident >>= \r -> evalExpr ctx (unevalExpr r) Unquote

        -- lists
        (AstList (Lst k elems), Eval) -> return $ ExprForm $ toForm k elems

        -- quote and unquote
        (_, Quote) -> evalExpr ctx (quote e) Eval
        (_, Unquote) -> unquote e >>= \e' -> evalExpr ctx e' Eval
        (_, DeepQuote) -> evalExpr ctx e Quote
        (_, DeepUnquote) -> evalExpr ctx e Unquote

unevalExpr :: PzVal -> AstExpr
unevalExpr = \case
    PzUnit -> AstList $ Lst KindForm []
    PzNum n -> AstNum n
    PzStr s -> AstStr s
    PzSymb s -> AstSymb s
    PzList l -> AstList $ Lst KindList $ map unevalExpr l
    PzDict m -> AstList $ Lst KindDict $ flip map (M.assocs m) $
        \(k, v) -> AstList $ Lst KindForm [unevalExpr k, unevalExpr v]
    PzFunc _ f ->
        case toFuncCustom f of
            Left ident -> AstIdent ident
            Right fc -> AstList $ Lst KindForm $ unevalFuncCustom fc

-- Utils
evalIdent :: Dict -> Ident -> Result PzVal
evalIdent ctx ident = case M.lookup (PzSymb $ symb ident) ctx of
    Just v -> Right v
    Nothing -> Left $
        "Error: Undefined identifier: " ++ show ident
        ++ "\n context keys: " ++ show (M.keys ctx)

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

type FuncReturn = Result (Dict, PzVal)
type EvalResult = Result Acc
type ReturnValue = Maybe PzVal
data Acc
    = Acc ReturnValue [StackFrame]
    deriving (Show, Eq)

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

invokeFuncSpecial :: Dict -> [AstExpr] -> [StackFrame] -> EvalResult
invokeFuncSpecial ctx es frames = do
    fc <- evalFuncCustom es
    let r = PzFunc ctx $ fromFuncCustom fc
    return $ Acc (Just r) frames

invokeFunc :: Dict -> Dict -> Func -> [PzVal] -> [StackFrame] -> EvalResult
invokeFunc ctx implCtx (Func impArgs args body) as frames =
    case body of
        BodyBuiltIn ident -> invokeFuncBuiltIn ctx as ident frames
        BodyCustom es -> invokeFuncCustom ctx as implCtx impArgs args es frames

invokeFuncCustom :: Dict -> [PzVal] -> Dict -> FuncImpureArgs -> FuncArgs -> [AstExpr] -> [StackFrame] -> EvalResult
invokeFuncCustom explCtx as implCtx impArgs args es frames =
    let toExpr = PzSymb .symb
        explCtxPairs = case impArgs of
            Both _ i -> [ (toExpr i, PzDict explCtx) ]
            _ -> []
       
        actLen = length as
        (expLen, argPairs) = case args of
            ArgsVaria i -> (actLen, [ (toExpr i, PzList as) ])
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

-- Utils
toAcc :: Dict -> AstExpr -> [StackFrame] -> ExprEvalResult -> EvalResult
toAcc ctx e frames r = case r of
    Evaled v -> return $ Acc (Just v) frames
    ExprForm es ->
        let mfi = case getIdent e of Left _ -> Nothing; Right fi -> Just fi in
        return $ Acc Nothing $ Form ctx mfi es : frames

returnFrom :: [StackFrame] -> FuncReturn -> EvalResult
returnFrom frames x = x >>= \(ctx, r) -> return $ Acc (Just r) $ setCtx ctx frames

-- Built-in functions
-- See module BuiltIns for implementations
invokeFuncBuiltIn :: Dict -> [PzVal] -> Ident -> [StackFrame] -> EvalResult
invokeFuncBuiltIn ctx args (Ident s) frames =
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