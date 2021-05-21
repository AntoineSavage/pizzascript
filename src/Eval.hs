{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Eval where

import qualified Data.Map as M

import Quote ( quote, unquote )
import BuiltIns ( withPos )
import Control.Monad ( forM_, liftM2 )
import Data.ArgPass ( ArgPass(..), argPassToSymb, symbToArgPass )
import Data.AstExpr ( AstExpr(..) )
import Data.FuncArgs ( FuncArgs(..) )
import Data.FuncCustom ( FuncCustom(..), toFuncCustom )
import Data.FuncImpureArgs ( FuncImpureArgs(..) )
import Data.Ident ( Ident )
import Data.Lst ( Lst(..), LstKind(..) )
import Data.Nat ( Nat(Z) )
import Data.Symb ( Symb(Symb), symb )
import Data.WithPos ( WithPos(WithPos, val), Pos )
import Types ( Dict, PzVal(..) )
import Utils ( getDuplicates, getIdent, toForm )

data ExprEvalResult
    = Evaled (WithPos PzVal)
    | ExprForm Pos [WithPos AstExpr]
    deriving (Show, Eq)

evalExpr :: Dict -> WithPos AstExpr -> ArgPass -> Either String ExprEvalResult
evalExpr ctx e@(WithPos p v) eval =
    let evaled = return . Evaled . WithPos p in
    case (v, eval) of
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
        (AstList (Lst k elems), Eval) -> return $ ExprForm p $ toForm p k elems

        -- quote and unquote
        (_, Quote) -> evalExpr ctx (quote e) Eval
        (_, Unquote) -> unquote e >>= \e' -> evalExpr ctx e' Eval
        (_, DeepQuote) -> evalExpr ctx e Quote
        (_, DeepUnquote) -> evalExpr ctx e Unquote

unevalExpr :: WithPos PzVal -> WithPos AstExpr
unevalExpr val = flip fmap val $ \case
    PzUnit -> AstList $ Lst KindForm []
    PzNum n -> AstNum n
    PzStr s -> AstStr s
    PzSymb s -> AstSymb s
    PzList l -> AstList $ Lst KindList $ map unevalExpr l
    PzDict m -> AstList $ Lst KindDict $ flip map (M.assocs m) $
        \(k, v) -> withPos $ AstList $ Lst KindForm [unevalExpr k, unevalExpr v]
    PzFunc _ f ->
        case toFuncCustom f of
            Left ident -> AstIdent ident
            Right fc -> AstList $ Lst KindForm $ unevalFuncCustom fc

evalFuncCustom :: [WithPos AstExpr] -> Either String FuncCustom
evalFuncCustom es0 = do
    (impArgs, es1) <- evalImpureArgs es0
    (args, body) <- evalArgs es1
    validateNoDuplicateIdents impArgs args
    return $ FuncCustom impArgs args body

unevalFuncCustom :: FuncCustom -> [WithPos AstExpr]
unevalFuncCustom (FuncCustom impArgs args body) = unevalImpureArgs impArgs ++ unevalArgs args ++ body

evalImpureArgs :: [WithPos AstExpr] -> Either String (FuncImpureArgs, [WithPos AstExpr])
evalImpureArgs elems = case elems of
    -- form starting with argument-passing behaviour symbol, followed by...
    WithPos p (AstList (Lst KindForm ((WithPos p2 (AstSymb s@(Symb Z _))):as))):es -> do
        argPass <- case symbToArgPass s of
            Just r -> return $ WithPos p2 r
            Nothing -> Left $
                "Error: Invalid argument-passing behaviour symbol: " ++ show s
                ++ "\n at: " ++ show p2

        case as of
            -- nothing else
            [] -> return (ArgPass p argPass, es)

            -- identifier
            [ e ] -> (,es) . Both p argPass <$> getIdent e

            _ -> Left $
                "Error: Impure function argument definition must be either:"
                    ++ "\n - a valid argument-passsing behaviour symbol only"
                    ++ "\n - a valid argument-passsing behaviour symbol, followed by an identifier"
                    ++ "\n was: " ++ show (map val elems)

    -- no match: assume no impure args
    _ -> return (None, elems)

unevalImpureArgs :: FuncImpureArgs -> [WithPos AstExpr]
unevalImpureArgs impArgs =
    let toExpr = fmap $ AstSymb .argPassToSymb
        toForm p = WithPos p . AstList . Lst KindForm
    in case impArgs of
        None -> []
        ArgPass p ap -> [ toForm p [toExpr ap] ]
        Both p ap ec -> [ toForm p [toExpr ap, fmap AstIdent ec] ]

evalArgs :: [WithPos AstExpr] -> Either String (FuncArgs, [WithPos AstExpr])
evalArgs elems = case elems of
    ie@(WithPos _ (AstIdent _)):es -> (,es) . ArgsVaria <$> getIdent ie
    WithPos p (AstList (Lst KindForm ies)):es -> (,es) . ArgsArity p <$> mapM getIdent ies
    _ -> Left $
        "Error: Function argument definition must be either:"
        ++ "\n - a single varargs identifier"
        ++ "\n - an form of arity identifiers"
        ++ "\n was: " ++ show (map val elems)

unevalArgs :: FuncArgs -> [WithPos AstExpr]
unevalArgs args =
    case args of
        ArgsVaria ident -> [fmap AstIdent ident]
        ArgsArity p is -> [WithPos p $ AstList $ Lst KindForm $ map (fmap AstIdent) is]

-- Utils
evalIdent :: Dict -> Ident -> Either String (WithPos PzVal)
evalIdent ctx ident = case M.lookup (withPos $ PzSymb $ symb ident) ctx of
    Just v -> Right v
    Nothing -> Left $
        "Error: Undefined identifier: " ++ show ident
        ++ "\n context keys: " ++ show (M.keys ctx)

validateNoDuplicateIdents :: FuncImpureArgs -> FuncArgs -> Either String ()
validateNoDuplicateIdents impArgs args =
    let explCtxIdents = case impArgs of
            Both _ _ i -> [i]
            _ -> []
       
        argIdents = case args of
            ArgsVaria i -> [i]
            ArgsArity _ is -> is
   
        duplicates = getDuplicates $ explCtxIdents ++ argIdents
    in if null duplicates
        then return ()
        else Left $
            "Error: Duplicate identifiers in function definition: " ++ show duplicates
