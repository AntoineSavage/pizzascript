module Eval where

import qualified Ast as A
import qualified Data.Map as M

import BuiltIns ( identList, identDict, identFunc )
import Control.Monad ( forM_ )
import Data.ArgPass ( ArgPass(Quote, Eval) )
import Data.Args ( Args, varargs )
import Data.Ident ( Ident(Ident), ident )
import Data.Maybe ( fromMaybe )
import Data.Nat ( Nat(..) )
import Data.Symb ( Symb(..), fromIdent )
import Text.Parsec ( SourcePos )

data PzVal
    = PzUnit
    | PzNum Double
    | PzStr String
    | PzSymb Symb
    | PzList [PzVal]
    | PzDict Dict
    | PzFunc ArgPass ImpureCtx Args FuncBody
    deriving (Show, Eq, Ord)

type Dict = M.Map PzVal PzVal

type ImpureCtx = Maybe Symb

data FuncBody
    = BuiltIn Ident
    | Custom [A.AstExpr]
    deriving (Show, Eq, Ord)

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
    let v' = evalExpr e
    print v'
    evalRec ctx v' es

evalExpr :: A.AstExpr -> PzVal
evalExpr (A.AstExpr p _ v) =
    case v of
        A.AstNum n -> PzNum n
        A.AstStr s -> PzStr s
        A.AstIdent i -> evalIdent p i
        A.AstSymb n i -> PzSymb $ Symb n i
        A.AstList k _ l -> evalList p k l

evalIdent :: SourcePos -> Ident -> PzVal
evalIdent p ident =
    let k = PzSymb $ fromIdent ident
    in case dictGet k ctx of
        PzUnit -> PzStr $ "undefined identifier: " ++ show ident ++ ", at: " ++ show p
        val -> val

evalList :: SourcePos -> A.ListKind -> [A.AstExpr] -> PzVal
evalList p k es =
    let toExpr ident = A.AstExpr p "" $ A.AstIdent ident in
    evalForm $ A.toForm p k es

evalForm :: [A.AstExpr] -> PzVal
evalForm [] = PzUnit
evalForm (f:as) =
    case evalExpr f of
        PzFunc _ _ _ (BuiltIn ident) -> invokeFunc ident as
        _ -> PzUnit

invokeFunc :: Ident -> [A.AstExpr] -> PzVal
invokeFunc (Ident ps) args =
    case ps of
        ["list"] -> list $ map evalExpr args
        ["dict"] -> dict $ map evalDictEntry args
        ["func"] -> PzUnit
        _ -> PzUnit

evalDictEntry :: A.AstExpr -> (PzVal, PzVal)
evalDictEntry (A.AstExpr _ _ v) =
    case v of
        (A.AstList A.KindForm _ [k, v]) ->
            (evalExpr k, evalExpr v)

        -- malformed dictionary entry
        _ -> (PzUnit, PzUnit)

-- Built-ins

-- numbers
numAdd :: PzVal -> PzVal -> PzVal
numAdd (PzNum n) (PzNum m) = PzNum $ n + m
numAdd _         _         = PzUnit

-- strings
-- TODO

-- symbols
-- TODO

-- lists
list :: [PzVal] -> PzVal
list = PzList

listHead :: PzVal -> PzVal
listHead (PzList (h:_)) = h
listHead _              = PzUnit -- invalid type or empty

listTail :: PzVal -> PzVal
listTail (PzList (_:t)) = PzList t
listTail _              = PzUnit -- invalid type or empty

-- dictionaries
dict :: [(PzVal, PzVal)] -> PzVal
dict es = PzDict $ M.fromList es

dictGet :: PzVal -> PzVal -> PzVal
dictGet k (PzDict m) = fromMaybe PzUnit $ M.lookup k m
dictGet _ _          = PzUnit -- Invalid type

dictPut :: PzVal -> PzVal -> PzVal -> PzVal
dictPut k v (PzDict m) = PzDict $ M.insert k v m
dictPut _ _ _          = PzUnit -- Invalid type

-- functions
-- TODO