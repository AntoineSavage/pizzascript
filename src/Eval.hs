module Eval where

import qualified Ast as A
import qualified Data.Map as M

import BuiltIns
import Control.Monad ( forM_, liftM2 )
import Data.Maybe ( fromMaybe )
import Data.Nat ( Nat(..) )
import Types

newtype Acc
    = Acc PzDict
    deriving (Show, Eq)

newAcc :: Ast -> Acc
newAcc (Ast _ es) = Acc M.empty 

exec :: Acc -> IO ()
exec (Acc ctx) = return ()

ctx :: PzVal
ctx = PzDict $ M.fromList
    [(PzSymb $ fromIdent identList, PzFunc Eval Nothing argsVariadic $ BuiltIn identList)
    , (PzSymb $ fromIdent identDict, PzFunc Quote Nothing argsVariadic $ BuiltIn identDict)
    , (PzSymb $ fromIdent identFunc, PzFunc Quote Nothing argsVariadic $ BuiltIn identFunc)
    ]

evalExpr :: AstExpr -> IO PzVal
evalExpr (AstExpr p _ v) =
    case v of
        AstNum n -> return $ PzNum n
        AstStr s -> return $ PzStr s
        AstIdent i -> evalIdent p i
        AstSymb n i -> return $ PzSymb $ Symb n i
        AstList k _ l -> evalList p k l

evalIdent :: AstPos -> Ident -> IO PzVal
evalIdent p ident = do
    let k = PzSymb $ fromIdent ident
    v <- dictGet k ctx
    return $ case v of
        PzUnit -> PzStr $ "undefined identifier: " ++ show ident ++ ", at: " ++ show p -- Error: undefined identifier
        val -> val

evalList :: AstPos -> ListKind -> [AstExpr] -> IO PzVal
evalList p k es =
    evalForm $ toForm p k es

evalForm :: [AstExpr] -> IO PzVal
evalForm [] = return PzUnit
evalForm (f:as) = do
    f' <- evalExpr f
    case f' of
        PzFunc _ _ _ (BuiltIn ident) -> invokeFunc ident as
        -- TODO: Invoke custom function
        _ -> return PzUnit -- Malformed function invocation

invokeFunc :: Ident -> [AstExpr] -> IO PzVal
invokeFunc (Ident ps) args =
    case ps of
        ["list"] -> mapM evalExpr args >>= list
        ["dict"] -> mapM evalDictEntry args >>= dict
        ["func"] -> return PzUnit
        _ -> return PzUnit -- Error: undefined identifier

evalDictEntry :: AstExpr -> IO (PzVal, PzVal)
evalDictEntry (AstExpr _ _ v) =
    case v of
        (AstList KindForm _ [k, v]) -> liftM2 (,) (evalExpr k) (evalExpr v)

        _ -> return (PzUnit, PzUnit) -- Error: malformed dictionary entry

list :: [PzVal] -> IO PzVal
list = return . PzList

dict :: [(PzVal, PzVal)] -> IO PzVal
dict es = return $ PzDict $ M.fromList es

dictGet :: PzVal -> PzVal -> IO PzVal
dictGet k (PzDict m) = return $ fromMaybe PzUnit $ M.lookup k m
dictGet _ _          = return PzUnit -- Error: invalid type