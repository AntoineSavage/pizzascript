module Eval where

import qualified Ast as A
import qualified Data.Map as M

import BuiltIns ( identList, identDict, identFunc )
import Control.Monad ( forM_, liftM2 )
import Data.ArgPass ( ArgPass(Quote, Eval) )
import Data.Args ( Args, varargs )
import Data.Ident ( Ident(Ident), ident )
import Data.Maybe ( fromMaybe )
import Data.Nat ( Nat(..) )
import Data.PzVal ( FuncBody(..), Dict, PzVal(..) )
import Data.Symb ( Symb(..), fromIdent )
import Text.Parsec ( SourcePos )

ctx :: PzVal
ctx = PzDict $ M.fromList
    [(PzSymb $ fromIdent identList, PzFunc Eval Nothing varargs $ BuiltIn identList)
    , (PzSymb $ fromIdent identDict, PzFunc Quote Nothing varargs $ BuiltIn identDict)
    , (PzSymb $ fromIdent identFunc, PzFunc Quote Nothing varargs $ BuiltIn identFunc)
    ]

evalMany :: [A.AstExpr] -> IO ()
evalMany es = do
    let ctx = M.empty
    (ctx', v) <- evalRec ctx PzUnit es
    print $ "Last ctx: " ++ show ctx'
    return ()

evalRec :: Dict -> PzVal -> [A.AstExpr] -> IO (Dict, PzVal)
evalRec ctx v []     = return (ctx, v)
evalRec ctx v (e:es) = do
    v' <- evalExpr e
    print v'
    evalRec ctx v' es

evalExpr :: A.AstExpr -> IO PzVal
evalExpr (A.AstExpr p _ v) =
    case v of
        A.AstNum n -> return $ PzNum n
        A.AstStr s -> return $ PzStr s
        A.AstIdent i -> evalIdent p i
        A.AstSymb n i -> return $ PzSymb $ Symb n i
        A.AstList k _ l -> evalList p k l

evalIdent :: SourcePos -> Ident -> IO PzVal
evalIdent p ident = do
    let k = PzSymb $ fromIdent ident
    v <- dictGet k ctx
    return $ case v of
        PzUnit -> PzStr $ "undefined identifier: " ++ show ident ++ ", at: " ++ show p -- Error: undefined identifier
        val -> val

evalList :: SourcePos -> A.ListKind -> [A.AstExpr] -> IO PzVal
evalList p k es =
    evalForm $ A.toForm p k es

evalForm :: [A.AstExpr] -> IO PzVal
evalForm [] = return PzUnit
evalForm (f:as) = do
    f' <- evalExpr f
    case f' of
        PzFunc _ _ _ (BuiltIn ident) -> invokeFunc ident as
        -- TODO: Invoke custom function
        _ -> return PzUnit -- Malformed function invocation

invokeFunc :: Ident -> [A.AstExpr] -> IO PzVal
invokeFunc (Ident ps) args =
    case ps of
        ["list"] -> mapM evalExpr args >>= list
        ["dict"] -> mapM evalDictEntry args >>= dict
        ["func"] -> return PzUnit
        _ -> return PzUnit -- Error: undefined identifier

evalDictEntry :: A.AstExpr -> IO (PzVal, PzVal)
evalDictEntry (A.AstExpr _ _ v) =
    case v of
        (A.AstList A.KindForm _ [k, v]) -> liftM2 (,) (evalExpr k) (evalExpr v)

        _ -> return (PzUnit, PzUnit) -- Error: malformed dictionary entry

list :: [PzVal] -> IO PzVal
list = return . PzList

dict :: [(PzVal, PzVal)] -> IO PzVal
dict es = return $ PzDict $ M.fromList es

dictGet :: PzVal -> PzVal -> IO PzVal
dictGet k (PzDict m) = return $ fromMaybe PzUnit $ M.lookup k m
dictGet _ _          = return PzUnit -- Error: invalid type