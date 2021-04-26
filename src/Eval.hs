module Eval where

import qualified Ast as A
import qualified Data.Map as M

import Control.Monad ( forM_ )
import Data.Maybe ( fromMaybe )
import Data.Nat ( Nat(..) )

data PzVal
    = PzUnit
    | PzNum Double
    | PzStr String
    | PzSymb Nat A.Ident
    | PzList [PzVal]
    | PzDict Dict
    | PzFunc ArgPass ImpureCtx Args FuncBody
    deriving (Show, Eq, Ord)

type Dict = M.Map PzVal PzVal

data ArgPass
    = Eval
    | Quote
    | Unquote
    | DeepQuote
    | DeepUnquote
    deriving (Show, Eq, Ord)

type ImpureCtx = Maybe A.Ident

data Args
    = Variadic A.Ident
    | Arity [A.Ident]
    deriving (Show, Eq, Ord)

data FuncBody
    = BuiltIn String
    | Custom [A.AstExpr]
    deriving (Show, Eq, Ord)

argPassToSymb :: M.Map ArgPass PzVal
argPassToSymb = M.map (PzSymb Z) $ M.fromList
    [ (Eval,        A.Ident ["eval"])
    , (Quote,       A.Ident ["quote"])
    , (Unquote,     A.Ident ["unquote"])
    , (DeepQuote,   A.Ident ["deep_quote"])
    , (DeepUnquote, A.Ident ["deep_unquote"])
    ]

varargs :: Args
varargs = Variadic $ A.Ident ["args"]

ctx :: PzVal
ctx = PzDict $ M.fromList
    [(PzSymb Z $ A.Ident ["list"], PzFunc Eval Nothing varargs $ BuiltIn "list")
    , (PzSymb Z $ A.Ident ["dict"], PzFunc Quote Nothing varargs $ BuiltIn "dict")
    , (PzSymb Z $ A.Ident ["func"], PzFunc Quote Nothing varargs $ BuiltIn "func")
    ]

evalAst :: A.Ast -> IO ()
evalAst (A.Ast _ es) = do
    forM_ es $ \e -> do
        print $ eval e

eval :: A.AstExpr -> PzVal
eval (A.AstExpr _ _ v) =
    case v of
        A.AstNum n -> PzNum n
        A.AstStr s -> PzStr s
        A.AstIdent i -> evalIdent i
        A.AstSymb n i -> PzSymb n i
        A.AstList k _ l -> evalList k l

evalIdent :: A.Ident -> PzVal
evalIdent ident =
    let k = PzSymb Z ident
    in case dictGet k ctx of
        PzUnit -> PzStr $ "undefined identifier: " ++ show ident
        val -> val

evalList :: A.ListKind -> [A.AstExpr] -> PzVal
evalList k es =
    case k of
        A.KindList -> PzList $ map eval es
        A.KindDict -> PzDict $ M.fromList $ map evalDictEntry es
        A.KindForm -> evalForm es

evalDictEntry :: A.AstExpr -> (PzVal, PzVal)
evalDictEntry (A.AstExpr _ _ v) =
    case v of
        (A.AstList A.KindForm _ [k, v]) ->
            (eval k, eval v)

        -- malformed dictionary entry
        _ -> (PzUnit, PzUnit)

evalForm :: [A.AstExpr] -> PzVal
evalForm [] = PzUnit
evalForm (f:as) =
    case eval f of
        PzFunc _ _ _ (BuiltIn s) -> evalFunc s as
        _ -> PzUnit

evalFunc :: String -> [A.AstExpr] -> PzVal
evalFunc name args =
    case name of
        "list" -> list $ map eval args
        "dict" -> dict $ map evalDictEntry args
        "func" -> PzUnit
        _ -> PzUnit


-- Built-in functions

-- number functions
numAdd :: PzVal -> PzVal -> PzVal
numAdd (PzNum n) (PzNum m) = PzNum $ n + m
numAdd _         _         = PzUnit

-- string functions
-- TODO

-- symbol functions
-- TODO

-- list functions
list :: [PzVal] -> PzVal
list = PzList

listHead :: PzVal -> PzVal
listHead (PzList (h:_)) = h
listHead _              = PzUnit -- invalid type or empty

listTail :: PzVal -> PzVal
listTail (PzList (_:t)) = PzList t
listTail _              = PzUnit -- invalid type or empty

-- dictionary functions
dict :: [(PzVal, PzVal)] -> PzVal
dict es = PzDict $ M.fromList es

dictGet :: PzVal -> PzVal -> PzVal
dictGet k (PzDict m) = fromMaybe PzUnit $ M.lookup k m
dictGet _ _          = PzUnit -- Invalid type

dictPut :: PzVal -> PzVal -> PzVal -> PzVal
dictPut k v (PzDict m) = PzDict $ M.insert k v m
dictPut _ _ _          = PzUnit -- Invalid type