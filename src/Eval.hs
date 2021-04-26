module Eval where

import qualified Ast as A
import qualified Ast.AstExpr as E
import qualified Ast.AstIdent as I
import qualified Ast.AstList as L
import qualified Ast.AstNum as N
import qualified Ast.AstStr as St
import qualified Ast.AstSymb as Sy
import qualified Data.Map as M

import Control.Monad ( forM_ )
import Data.Maybe ( fromMaybe )
import Data.Nat ( Nat(..) )

data PzVal
    = PzUnit
    | PzNum Double
    | PzStr String
    | PzSymb Nat I.AstIdent
    | PzList [PzVal]
    | PzDict (M.Map PzVal PzVal)
    | PzFunc String -- TODO
    deriving (Show, Eq, Ord)

ident_a :: I.AstIdent
ident_a = I.AstIdent (I.AstIdentPart 'a' "") []
ident_b :: I.AstIdent
ident_b = I.AstIdent (I.AstIdentPart 'b' "") []
ident_c :: I.AstIdent
ident_c = I.AstIdent (I.AstIdentPart 'c' "") []
ident_abc :: I.AstIdent
ident_abc = I.AstIdent (I.AstIdentPart 'a' "bc") []
ident__foo :: I.AstIdent
ident__foo = I.AstIdent (I.AstIdentPart '_' "foo") []
ident_BAR99 :: I.AstIdent
ident_BAR99 = I.AstIdent (I.AstIdentPart 'B' "AR99") []
ident_B4Z :: I.AstIdent
ident_B4Z = I.AstIdent (I.AstIdentPart 'B' "4Z") []
ident_true :: I.AstIdent
ident_true = I.AstIdent (I.AstIdentPart 't' "rue") []
ident_false :: I.AstIdent
ident_false = I.AstIdent (I.AstIdentPart 'f' "alse") []
ident_Module_function :: I.AstIdent
ident_Module_function = I.AstIdent (I.AstIdentPart 'M' "odule") [I.AstIdentPart 'f' "unction"]
ident_Package_Module_function :: I.AstIdent
ident_Package_Module_function = I.AstIdent (I.AstIdentPart 'P' "ackage") [I.AstIdentPart 'M' "odule",I.AstIdentPart 'f' "unction"]
ident_Package_Module_Submodule :: I.AstIdent
ident_Package_Module_Submodule = I.AstIdent (I.AstIdentPart 'P' "ackage") [I.AstIdentPart 'M' "odule",I.AstIdentPart 'S' "ubModule"]
ident_my_dict_my_key1_my_key2 :: I.AstIdent
ident_my_dict_my_key1_my_key2 = I.AstIdent (I.AstIdentPart 'm' "y_dict") [I.AstIdentPart 'm' "y_key1",I.AstIdentPart 'm' "y_key2"]
ident_list :: I.AstIdent
ident_list = I.AstIdent (I.AstIdentPart 'l' "ist") []
ident_dict :: I.AstIdent
ident_dict = I.AstIdent (I.AstIdentPart 'd' "ict") []
ident_func :: I.AstIdent
ident_func = I.AstIdent (I.AstIdentPart 'f' "unc") []

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

eval :: E.AstExpr -> PzVal
eval (E.AstExpr _ _ v) =
    case v of
        E.ValNum n -> evalNum n
        E.ValStr s -> evalStr s
        E.ValIdent i -> evalIdent i
        E.ValSymb s -> evalSymb s
        E.ValList l -> evalList l

evalNum :: N.AstNum -> PzVal
evalNum (N.AstNum n) = PzNum n

evalStr :: St.AstStr -> PzVal
evalStr (St.AstStr s) = PzStr s

evalIdent :: I.AstIdent -> PzVal
evalIdent ident =
    let k = PzSymb Z ident
    in case dictGet k ctx of
        PzUnit -> PzStr $ "undefined identifier: " ++ show ident
        val -> val

evalSymb :: Sy.AstSymb -> PzVal
evalSymb (Sy.AstSymb n i) = PzSymb n i

evalList :: L.AstList E.AstExpr -> PzVal
evalList (L.AstList k _ es) =
    case k of
        L.KindList -> PzList $ map eval es
        L.KindDict -> PzDict $ M.fromList $ map evalDictEntry es
        L.KindForm -> evalForm es

evalDictEntry :: E.AstExpr -> (PzVal, PzVal)
evalDictEntry (E.AstExpr _ _ v) =
    case v of
        (E.ValList (L.AstList L.KindForm _ [k, v])) ->
            (eval k, eval v)

        -- malformed dictionary entry
        _ -> (PzUnit, PzUnit)

evalForm :: [E.AstExpr] -> PzVal
evalForm [] = PzUnit
evalForm (f:as) =
    case eval f of
        PzFunc s -> evalFunc s as

        -- malformed form
        _ -> PzUnit

evalFunc :: String -> [E.AstExpr] -> PzVal
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