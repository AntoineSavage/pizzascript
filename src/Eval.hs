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
    | PzDict (M.Map PzVal PzVal)
    | PzFunc String -- TODO
    deriving (Show, Eq, Ord)

ident_a :: A.Ident
ident_a = A.Ident ["a"]
ident_b :: A.Ident
ident_b = A.Ident ["b"]
ident_c :: A.Ident
ident_c = A.Ident ["c"]
ident_abc :: A.Ident
ident_abc = A.Ident ["abc"]
ident__foo :: A.Ident
ident__foo = A.Ident ["_foo"]
ident_BAR99 :: A.Ident
ident_BAR99 = A.Ident ["BAR99"]
ident_B4Z :: A.Ident
ident_B4Z = A.Ident ["B4Z"]
ident_true :: A.Ident
ident_true = A.Ident ["true"]
ident_false :: A.Ident
ident_false = A.Ident ["false"]
ident_Module_function :: A.Ident
ident_Module_function = A.Ident ["Module", "function"]
ident_Package_Module_function :: A.Ident
ident_Package_Module_function = A.Ident ["Package", "Module", "function"]
ident_Package_Module_Submodule :: A.Ident
ident_Package_Module_Submodule = A.Ident ["Package", "Module", "SubModule"]
ident_my_dict_my_key1_my_key2 :: A.Ident
ident_my_dict_my_key1_my_key2 = A.Ident ["my_dict", "my_key1", "my_key2"]
ident_list :: A.Ident
ident_list = A.Ident ["list"]
ident_dict :: A.Ident
ident_dict = A.Ident ["dict"]
ident_func :: A.Ident
ident_func = A.Ident ["func"]

symb_a :: PzVal
symb_a = PzSymb Z ident_a
symb_b :: PzVal
symb_b = PzSymb (S Z) ident_b
symb_c :: PzVal
symb_c = PzSymb (S Z) ident_c
symb_abc :: PzVal
symb_abc = PzSymb Z ident_abc
symb__foo :: PzVal
symb__foo = PzSymb Z ident__foo
symb_BAR99 :: PzVal
symb_BAR99 = PzSymb (S Z) ident_BAR99
symb_B4Z :: PzVal
symb_B4Z = PzSymb (S (S Z)) ident_B4Z
symb_true :: PzVal
symb_true = PzSymb Z ident_true
symb_false :: PzVal
symb_false = PzSymb Z ident_false
symb_Module_function :: PzVal
symb_Module_function = PzSymb Z ident_Module_function
symb_Package_Module_function :: PzVal
symb_Package_Module_function = PzSymb (S Z) ident_Package_Module_function
symb_Package_Module_Submodule :: PzVal
symb_Package_Module_Submodule = PzSymb (S (S Z)) ident_Package_Module_Submodule
symb_my_dict_my_key1_my_key2 :: PzVal
symb_my_dict_my_key1_my_key2 = PzSymb (S (S (S Z))) ident_my_dict_my_key1_my_key2
symb_list :: PzVal
symb_list = PzSymb Z ident_list
symb_dict :: PzVal
symb_dict = PzSymb Z ident_dict
symb_func :: PzVal
symb_func = PzSymb Z ident_func

ctx :: PzVal
ctx = PzDict $ M.fromList
    [ (symb_a, symb_a)
    , (symb_b, symb_b)
    , (symb_c, symb_c)
    , (symb_abc, symb_abc)
    , (symb__foo, symb__foo)
    , (symb_BAR99, symb_BAR99)
    , (symb_B4Z, symb_B4Z)
    , (symb_true, symb_true)
    , (symb_false, symb_false)
    , (symb_Module_function, symb_Module_function)
    , (symb_Package_Module_function, symb_Package_Module_function)
    , (symb_Package_Module_Submodule, symb_Package_Module_Submodule)
    , (symb_my_dict_my_key1_my_key2, symb_my_dict_my_key1_my_key2)
    , (symb_list, PzFunc "list")
    , (symb_dict, PzFunc "dict")
    , (symb_func, PzFunc "func")
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
        PzFunc s -> evalFunc s as

        -- malformed form
        _ -> PzUnit

evalFunc :: String -> [A.AstExpr] -> PzVal
evalFunc name astArgs =
    case name of
        "list" -> list $ map eval astArgs
        "dict" -> dict $ map evalDictEntry astArgs
        "func" -> PzFunc $ name ++ ": " ++ show astArgs
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