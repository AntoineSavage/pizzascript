{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Eval where

import qualified Ast as A
import qualified Data.Map as M

import Quote ( quote, unquote )
import BuiltIns ( withPos )
import Control.Monad ( forM_, liftM2 )
import Data.Nat ( Nat(..) )
import Types
import Utils

type EvalResult = Either String Acc

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

evalFuncCustom :: [WithPos AstExpr] -> Either String FuncCustom
evalFuncCustom es0 = do
    (impArgs, es1) <- evalImpureArgs es0
    (args, es2) <- evalArgs es1
    validateNoDuplicateIdents impArgs args
    return $ FuncCustom impArgs args es2

unevalFuncCustom :: FuncCustom -> [WithPos AstExpr]
unevalFuncCustom (FuncCustom impArgs args body) = unevalImpureArgs impArgs ++ unevalArgs args ++ body

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

unevalImpureArgs :: FuncImpureArgs -> [WithPos AstExpr]
unevalImpureArgs impArgs =
    let toExpr = fmap $ AstSymb .argPassToSymb
        toForm p = WithPos p . AstList KindForm
    in case impArgs of
        None -> []
        ArgPass p ap -> [ toForm p [toExpr ap] ]
        Both p ap ec -> [ toForm p [toExpr ap, fmap (AstSymb .symb) ec] ]

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

unevalArgs :: FuncArgs -> [WithPos AstExpr]
unevalArgs args =
    case args of
        ArgsVaria ident -> [fmap AstIdent ident]
        ArgsArity is -> map (fmap AstIdent) is

-- Utils
evalIdent :: Dict -> Pos -> Ident -> Either String (WithPos PzVal)
evalIdent ctx p ident = inner (withPos $ PzDict ctx) $ splitSymb $ symb ident where
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
