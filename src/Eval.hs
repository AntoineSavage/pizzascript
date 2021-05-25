{-# LANGUAGE LambdaCase, TupleSections #-}
module Eval where

import qualified Data.Map as M

import Data.Func.ArgPass ( ArgPass(..), argPassToSymb, symbToArgPass )
import Data.Func.FuncArgs ( FuncArgs(..) )
import Data.Func.FuncCustom ( toFuncCustom, FuncCustom(..) )
import Data.Func.FuncImpureArgs ( FuncImpureArgs(..) )
import Data.Nat ( Nat(..) )
import Data.PzVal ( Dict, PzVal(..), pd, pl )
import Data.Symb ( Symb(..), quoteSymb, unparseSymb )
import Utils ( Result, getDuplicates, unparse )

uneval :: PzVal -> PzVal
uneval = \case
    PzUnit -> PzList []
    PzNum n -> PzNum n
    PzStr s -> PzStr s
    PzSymb s -> PzSymb $ quoteSymb s
    PzList l -> PzList $ (pl:) $ map uneval l
    PzDict m -> PzList $ (pd:) $ flip map (M.assocs m) $ \(k, v) -> PzList [uneval k, uneval v]
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
evalQuotedIdent ctx k = case M.lookup k ctx of
    Just v -> Right v
    Nothing -> Left $
        "Error: Undefined identifier: " ++ unparse k
        ++ "\n context keys: " ++ show (map unparse $ M.keys ctx)

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
            "Error: Duplicate identifiers in function definition: " ++ show duplicates

unconsFuncBody :: [PzVal] -> Result (PzVal, [PzVal])
unconsFuncBody = \case
    (x:xs) -> return (x, xs)
    _ -> Left "Error: Function body must not be empty"

getQuotedIdent :: PzVal -> Result Symb
getQuotedIdent v = case v of
    PzSymb s@(Symb Z _ _) -> return s
    _ -> Left $ "Expected identifier"
        ++ "\n was: " ++ show v -- TODO pretty