module Eval where

import qualified Ast as A
import qualified Data.Map as M

import BuiltIns
import Control.Monad ( forM_, liftM2 )
import Data.Maybe ( fromMaybe )
import Data.Nat ( Nat(..) )
import Types

data Acc
    = Acc PzVal Dict Stack
    deriving (Show, Eq)

data Stack
    = Block [AstExpr] (Maybe Stack)
    | Form AstPos [AstExpr] Stack
    | Invoc AstPos Func [PzVal] [AstExpr] Stack
    deriving (Show, Eq)

evalAst :: Ast -> IO ()
evalAst (Ast _ es) = go $ Acc PzUnit M.empty $ Block es Nothing

go :: Acc -> IO ()
go (Acc last ctx stack) =
    case stack of
        Block [] Nothing ->
            -- empty stack: halt
            return ()
        
        Block [] (Just s) ->
            -- return from block

        Block (e:es) s ->
            -- evaluate expression
            case evalExpr last ctx e $ Block es s of
                Left s -> putStrLn s
                Right acc -> go acc

        Form _ [] s ->
            -- empty form -> unit type
            reduce $ Acc PzUnit ctx s
        
        Form p (f:as) s -> putStrLn "TODO: Go Form"

        _ -> putStrLn "TODO: Go _"

evalExpr :: PzVal -> Dict -> AstExpr -> Stack -> Either String Acc
evalExpr last ctx (AstExpr p _ v) stack = 
    let set next = return $ Acc next ctx stack in
    case v of
        -- num, str and symb evaluate to themselves
        AstNum n -> set $ PzNum n
        AstStr s -> set $ PzStr s
        AstSymb symb -> set $  PzSymb symb

        -- replace identifier with corresponding value from ctx
        AstIdent ident ->
            case dictGet (PzSymb $ fromIdent ident) ctx of
                PzUnit -> Left $ "Error: Undefined identifier: " ++ show ident ++ ", at: " ++ show p
                val -> set val

        -- push form on stack
        AstList k _ elems -> return $ Acc last ctx $ Form p (toForm p k elems) stack

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

list :: [PzVal] -> PzVal
list = PzList

dict :: Dict -> PzVal
dict = PzDict

dictGet :: PzVal -> Dict -> PzVal
dictGet k m = fromMaybe PzUnit $ M.lookup k m