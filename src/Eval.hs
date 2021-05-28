{-# LANGUAGE LambdaCase, TupleSections #-}
module Eval where

import qualified Data.Map as M

import Ops.Func.ArgPass ( argPassToSymb, symbToArgPass )
import Ops.Func.FuncCustom ( toFuncCustom )
import Ops.Symb ( quoteSymb, unparseSymb, unquoteSymb )
import Symbs ( pzSymbDict, pzSymbList )
import Types.Func.FuncArgs ( FuncArgs(..) )
import Types.Func.FuncCustom ( FuncCustom(..) )
import Types.Func.FuncImpureArgs ( FuncImpureArgs(..) )
import Types.Nat ( Nat(..) )
import Types.PzVal ( Dict, DictKey(..), PzVal(..) )
import Types.Symb ( Symb(..) )
import Utils ( Result, getDuplicates, unparse )

data EvalResult
    = Evaled PzVal
    | Form PzVal [PzVal]
    deriving (Show, Eq)

eval :: Dict -> PzVal -> Result EvalResult
eval ctx v = let evaled = return . Evaled in case v of
    PzNum _ -> evaled v
    PzStr _ -> evaled v
    PzSymb s -> case s of
        Symb Z _ _  -> Evaled <$> evalQuotedIdent ctx v
        _           -> evaled $ PzSymb $ unquoteSymb s
    PzList l -> case l of
        []  -> evaled PzUnit
        (x:xs)   -> return $ Form x xs
    _ -> error $ "Can only evaluate quoted values: " ++ show v

uneval :: PzVal -> PzVal
uneval v = case v of
    PzUnit -> PzList []
    PzNum _ -> v
    PzStr _ -> v
    PzSymb s -> PzSymb $ quoteSymb s
    PzList l -> PzList $ (pzSymbList:) $ map uneval l
    PzDict m -> PzList $ (pzSymbDict:) $ flip map (M.assocs m) $ \(DictKey k, v) -> PzList [uneval k, uneval v]
    PzFunc _ f -> case toFuncCustom f of
        Left s -> PzSymb s
        Right fc -> PzList $ unevalFuncCustom fc

evalFuncCustom :: [PzVal] -> Result FuncCustom
evalFuncCustom es0 = do
    (impArgs, es1) <- evalImpureArgs es0
    (args, es2) <- evalArgs es1
    validateNoDuplicateQuotedIdents impArgs args
    (x, xs) <- unconsFuncBody es2
    return $ FuncCustom impArgs args x xs

unevalFuncCustom :: FuncCustom -> [PzVal]
unevalFuncCustom (FuncCustom impArgs args x xs) = unevalImpureArgs impArgs ++ unevalArgs args ++ [x] ++ xs

evalImpureArgs :: [PzVal] -> Result (FuncImpureArgs, [PzVal])
evalImpureArgs elems = case elems of
    -- form starting with argument-passing behaviour symbol, followed by...
    PzList ((PzSymb s@(Symb (S Z) _ _)):as):es -> do
        argPass <- case symbToArgPass s of
            Just r -> return r
            Nothing -> Left $
                "Error: Invalid argument-passing behaviour symbol: " ++ show s

        case as of
            -- nothing else
            [] -> return (ArgPass argPass, es)

            -- identifier
            [ e ] -> (,es) . Both argPass <$> getQuotedIdent e

            _ -> Left $
                "Error: Impure function argument definition must be either:"
                    ++ "\n - a valid argument-passsing behaviour symbol only"
                    ++ "\n - a valid argument-passsing behaviour symbol, followed by an identifier"
                    ++ "\n was: " ++ show elems

    -- no match: assume no impure args
    _ -> return (None, elems)

unevalImpureArgs :: FuncImpureArgs -> [PzVal]
unevalImpureArgs impArgs =
    let toExpr = PzSymb . argPassToSymb
    in case impArgs of
        None -> []
        ArgPass ap -> [ PzList [toExpr ap] ]
        Both ap ec -> [ PzList [toExpr ap, PzSymb ec] ]

evalArgs :: [PzVal] -> Result (FuncArgs, [PzVal])
evalArgs elems = case elems of
    v@(PzSymb _) :es -> (,es) . ArgsVaria <$> getQuotedIdent v
    PzList vs    :es -> (,es) . ArgsArity <$> mapM getQuotedIdent vs
    _ -> Left $
        "Error: Function argument definition must be either:"
        ++ "\n - a single varargs identifier"
        ++ "\n - an form of arity identifiers"
        ++ "\n was: " ++ show elems

unevalArgs :: FuncArgs -> [PzVal]
unevalArgs = \case
    ArgsVaria s -> [PzSymb s]
    ArgsArity ss -> [PzList $ map PzSymb ss]

-- Utils
evalQuotedIdent :: Dict -> PzVal -> Result PzVal
evalQuotedIdent ctx k = case M.lookup (DictKey k) ctx of
    Just v -> Right v
    Nothing -> Left $
        "Error: Undefined identifier: " ++ unparse k
            ++ "\n context keys: " ++ show (M.keys ctx)

validateNoDuplicateQuotedIdents :: FuncImpureArgs -> FuncArgs -> Result ()
validateNoDuplicateQuotedIdents impArgs args =
    let explCtxQuotedIdents = case impArgs of
            Both _ s -> [s]
            _ -> []
     
        argQuotedIdents = case args of
            ArgsVaria s -> [s]
            ArgsArity ss -> ss
 
        duplicates = getDuplicates $ explCtxQuotedIdents ++ argQuotedIdents
    in if null duplicates
        then return ()
        else Left $
            "Error: Duplicate identifiers in function definition: "
                ++ concatMap unparseSymb duplicates

unconsFuncBody :: [PzVal] -> Result (PzVal, [PzVal])
unconsFuncBody = \case
    (x:xs) -> return (x, xs)
    _ -> Left "Error: Function body must not be empty"

getQuotedIdent :: PzVal -> Result Symb
getQuotedIdent v = case v of
    PzSymb s@(Symb Z _ _) -> return s
    _ -> Left $ "Expected identifier"
        ++ "\n was: " ++ show v