{-# LANGUAGE LambdaCase, TupleSections #-}
module Eval where

import Data.Func.FuncArgs ( FuncArgs(..) )
import Data.Nat ( Nat(Z) )
import Data.PzVal ( PzVal(PzSymb, PzList) )
import Data.Symb ( Symb(..) )
import Utils ( Result )

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