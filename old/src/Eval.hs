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

evalFuncCustom :: [AstExpr] -> Result FuncCustom
evalFuncCustom es0 = do
    (impArgs, es1) <- evalImpureArgs es0
    (args, body) <- evalArgs es1
    validateNoDuplicateIdents impArgs args
    return $ FuncCustom impArgs args body

unevalFuncCustom :: FuncCustom -> [AstExpr]
unevalFuncCustom (FuncCustom impArgs args body) = unevalImpureArgs impArgs ++ unevalArgs args ++ body

evalImpureArgs :: [AstExpr] -> Result (FuncImpureArgs, [AstExpr])
evalImpureArgs elems = case elems of
    -- form starting with argument-passing behaviour symbol, followed by...
    AstList (Lst KindForm ((AstSymb s@(Symb Z _)):as)):es -> do
        argPass <- case symbToArgPass s of
            Just r -> return r
            Nothing -> Left $
                "Error: Invalid argument-passing behaviour symbol: " ++ show s

        case as of
            -- nothing else
            [] -> return (ArgPass argPass, es)

            -- identifier
            [ e ] -> (,es) . Both argPass <$> getIdent e

            _ -> Left $
                "Error: Impure function argument definition must be either:"
                    ++ "\n - a valid argument-passsing behaviour symbol only"
                    ++ "\n - a valid argument-passsing behaviour symbol, followed by an identifier"
                    ++ "\n was: " ++ show elems

    -- no match: assume no impure args
    _ -> return (None, elems)

unevalImpureArgs :: FuncImpureArgs -> [AstExpr]
unevalImpureArgs impArgs =
    let toExpr = AstSymb .argPassToSymb
        toForm = AstList . Lst KindForm
    in case impArgs of
        None -> []
        ArgPass ap -> [ toForm [toExpr ap] ]
        Both ap ec -> [ toForm [toExpr ap, AstIdent ec] ]

evalArgs :: [AstExpr] -> Result (FuncArgs, [AstExpr])
evalArgs elems = case elems of
    ie@(AstIdent _):es -> (,es) . ArgsVaria <$> getIdent ie
    AstList (Lst KindForm ies):es -> (,es) . ArgsArity <$> mapM getIdent ies
    _ -> Left $
        "Error: Function argument definition must be either:"
        ++ "\n - a single varargs identifier"
        ++ "\n - an form of arity identifiers"
        ++ "\n was: " ++ show elems

unevalArgs :: FuncArgs -> [AstExpr]
unevalArgs args =
    case args of
        ArgsVaria ident -> [AstIdent ident]
        ArgsArity is -> [AstList $ Lst KindForm $ map AstIdent is]

-- Utils
evalIdent :: Dict -> Ident -> Result PzVal
evalIdent ctx ident = case M.lookup (PzSymb $ symb ident) ctx of
    Just v -> Right v
    Nothing -> Left $
        "Error: Undefined identifier: " ++ show ident
        ++ "\n context keys: " ++ show (M.keys ctx)

validateNoDuplicateIdents :: FuncImpureArgs -> FuncArgs -> Result ()
validateNoDuplicateIdents impArgs args =
    let explCtxIdents = case impArgs of
            Both _ i -> [i]
            _ -> []
       
        argIdents = case args of
            ArgsVaria i -> [i]
            ArgsArity is -> is
   
        duplicates = getDuplicates $ explCtxIdents ++ argIdents
    in if null duplicates
        then return ()
        else Left $
            "Error: Duplicate identifiers in function definition: " ++ show duplicates
