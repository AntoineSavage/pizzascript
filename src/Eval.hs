{-# LANGUAGE LambdaCase, TupleSections #-}
module Eval where

import Data.Func.ArgPass
import Data.Func.FuncArgs ( FuncArgs(..) )
import Data.Func.FuncImpureArgs
import Data.Nat ( Nat(Z) )
import Data.PzVal ( PzVal(PzSymb, PzList) )
import Data.Symb ( Symb(..) )
import Utils ( Result )

evalImpureArgs :: [PzVal] -> Result (FuncImpureArgs, [PzVal])
evalImpureArgs elems = case elems of
    -- form starting with argument-passing behaviour symbol, followed by...
    PzList ((PzSymb s@(Symb Z _ _)):as):es -> do
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
getQuotedIdent :: PzVal -> Result Symb
getQuotedIdent v = case v of
    PzSymb s@(Symb Z _ _) -> return s
    _ -> Left $ "Expected identifier"
        ++ "\n was: " ++ show v -- TODO pretty