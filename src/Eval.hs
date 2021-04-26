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
    = ValUnit
    | ValNum PzNum
    | ValStr PzStr
    | ValSymb PzSymb
    | ValList PzList
    | ValDict PzDict
    | ValFunc PzFunc
    deriving (Show, Eq, Ord)

data PzNum
    = PzInteger Integer
    | PzDouble Double
    deriving (Show, Eq, Ord)

newtype PzStr =
    PzStr String
    deriving (Show, Eq, Ord)

data PzSymb
    = PzSymb Nat I.AstIdent
    deriving (Show, Eq, Ord)

newtype PzList
    = PzList [PzVal]
    deriving (Show, Eq, Ord)

newtype PzDict
    = PzDict (M.Map PzVal PzVal)
    deriving (Show, Eq, Ord)

data PzFunc
    = PzFunc String -- TODO
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
symb_a = ValSymb $ PzSymb Z ident_a
symb_b :: PzVal
symb_b = ValSymb $ PzSymb (S Z) ident_b
symb_c :: PzVal
symb_c = ValSymb $ PzSymb (S Z) ident_c
symb_abc :: PzVal
symb_abc = ValSymb $ PzSymb Z ident_abc
symb__foo :: PzVal
symb__foo = ValSymb $ PzSymb Z ident__foo
symb_BAR99 :: PzVal
symb_BAR99 = ValSymb $ PzSymb (S Z) ident_BAR99
symb_B4Z :: PzVal
symb_B4Z = ValSymb $ PzSymb (S (S Z)) ident_B4Z
symb_true :: PzVal
symb_true = ValSymb $ PzSymb Z ident_true
symb_false :: PzVal
symb_false = ValSymb $ PzSymb Z ident_false
symb_Module_function :: PzVal
symb_Module_function = ValSymb $ PzSymb Z ident_Module_function
symb_Package_Module_function :: PzVal
symb_Package_Module_function = ValSymb $ PzSymb (S Z) ident_Package_Module_function
symb_Package_Module_Submodule :: PzVal
symb_Package_Module_Submodule = ValSymb $ PzSymb (S (S Z)) ident_Package_Module_Submodule
symb_my_dict_my_key1_my_key2 :: PzVal
symb_my_dict_my_key1_my_key2 = ValSymb $ PzSymb (S (S (S Z))) ident_my_dict_my_key1_my_key2
symb_list :: PzVal
symb_list = ValSymb $ PzSymb Z ident_list
symb_dict :: PzVal
symb_dict = ValSymb $ PzSymb Z ident_dict
symb_func :: PzVal
symb_func = ValSymb $ PzSymb Z ident_func

ctx :: PzDict
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
    , (symb_list, ValFunc $ PzFunc "list")
    , (symb_dict, ValFunc $ PzFunc "dict")
    , (symb_func, ValFunc $ PzFunc "func")
    ]

evalAst :: A.Ast -> IO ()
evalAst (A.Ast _ es) = do
    forM_ es $ \e -> do
        print $ eval e

eval :: E.AstExpr -> PzVal
eval (E.AstExpr _ _ v) =
    case v of
        E.ValNum n -> ValNum $ evalNum n
        E.ValStr s -> ValStr $ evalStr s
        E.ValIdent i -> evalIdent i
        E.ValSymb s -> ValSymb $ evalSymb s
        E.ValList l -> evalList l

evalNum :: N.AstNum -> PzNum
evalNum (N.AstInteger n) = PzInteger n
evalNum (N.AstDouble d) = PzDouble d

evalStr :: St.AstStr -> PzStr
evalStr (St.AstStr s) = PzStr s

evalIdent :: I.AstIdent -> PzVal
evalIdent ident =
    let k = ValSymb $ PzSymb Z ident
    in case dictGet k (ValDict ctx) of
        ValUnit -> ValStr $ PzStr $ "undefined identifier: " ++ show ident
        val -> val

evalSymb :: Sy.AstSymb -> PzSymb
evalSymb (Sy.AstSymb n i) = PzSymb n i

evalList :: L.AstList E.AstExpr -> PzVal
evalList (L.AstList k _ es) =
    case k of
        L.KindList -> ValList $ PzList $ map eval es
        L.KindDict -> ValDict $ PzDict $ M.fromList $ map evalDictEntry es
        L.KindForm -> evalForm es

evalDictEntry :: E.AstExpr -> (PzVal, PzVal)
evalDictEntry (E.AstExpr _ _ v) =
    case v of
        (E.ValList (L.AstList L.KindForm _ [k, v])) ->
            (eval k, eval v)

        -- malformed dictionary entry
        _ -> (ValUnit, ValUnit)

evalForm :: [E.AstExpr] -> PzVal
evalForm [] = ValUnit
evalForm (f:as) =
    case eval f of
        ValFunc (PzFunc s) -> evalFunc s as

        -- malformed form
        _ -> ValUnit

evalFunc :: String -> [E.AstExpr] -> PzVal
evalFunc name astArgs =
    case name of
        "list" -> list $ map eval astArgs
        "dict" -> dict $ map evalDictEntry astArgs
        "func" -> ValFunc $ PzFunc $ name ++ ": " ++ show astArgs
        _ -> ValUnit


-- Built-in functions

-- number functions
numAdd :: PzVal -> PzVal -> PzVal
numAdd (ValNum (PzInteger n)) (ValNum (PzInteger m)) = ValNum $ PzInteger $ n + m
numAdd _                      _                      = ValUnit

-- string functions
-- TODO

-- symbol functions
-- TODO

-- list functions
list :: [PzVal] -> PzVal
list = ValList . PzList

listHead :: PzVal -> PzVal
listHead (ValList (PzList (h:_))) = h
listHead _                        = ValUnit -- invalid type or empty

listTail :: PzVal -> PzVal
listTail (ValList (PzList (_:t))) = ValList $ PzList t
listTail _                        = ValUnit -- invalid type or empty

-- dictionary functions
dict :: [(PzVal, PzVal)] -> PzVal
dict es = ValDict $ PzDict $ M.fromList es

dictGet :: PzVal -> PzVal -> PzVal
dictGet k (ValDict (PzDict m)) = fromMaybe ValUnit $ M.lookup k m
dictGet _ _                    = ValUnit -- Invalid type

dictPut :: PzVal -> PzVal -> PzVal -> PzVal
dictPut k v (ValDict (PzDict m)) = ValDict $ PzDict $ M.insert k v m
dictPut _ _ _                    = ValUnit -- Invalid type