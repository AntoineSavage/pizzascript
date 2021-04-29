module Eval where

import qualified Ast as A
import qualified Data.Map as M

import BuiltIns
import Control.Monad ( forM_, liftM2 )
import Data.Maybe ( fromMaybe )
import Data.Nat ( Nat(..) )
import Types

data Acc
    = Acc (Maybe PzVal) Dict [StackFrame]
    deriving (Show, Eq)

data StackFrame
    = Block [AstExpr]
    | Form AstPos [AstExpr]
    | Invoc AstPos Func [PzVal] [AstExpr]
    deriving (Show, Eq)

evalAst :: Ast -> IO ()
evalAst (Ast _ es) = go $ Acc Nothing M.empty [Block es]

go :: Acc -> IO ()
go (Acc result ctx []) = return () -- halt
go (Acc result ctx (frame:frames)) =
    case (frame, result) of
        (Block [], _) ->
            -- block finished: pop stack
            go $ Acc result ctx fs
        
        (Block (e:es), _) ->
            -- evaluate expression
            case evalExpr ctx e $ Block es:fs of
                Left s -> putStrLn s
                Right acc -> go acc

        (Form _ [], Nothing) ->
            -- empty form -> unit type
            go $ Acc (Just PzUnit) ctx fs
        
        (Form p (f:as), Nothing) ->
            -- evaluate first form elem (i.e. the function)
            case evalExpr ctx f $ Form p as of
                Left s -> putStrLn $ s ++ "\n at: " ++ show p
                Right acc -> go acc
        
        (Form p as, Just f) ->
            case f of
            case evalExpr ctx f [] of
                Left s -> putStrLn $ s ++ "\n at: " ++ show p
                Right acc@(Acc mresult ) -> go acc

        _ -> putStrLn "TODO: Go _"

-- Returns Left if error
-- Sets Just result if not form
-- Sets Nothing and push form otherwise
evalExpr :: Dict -> AstExpr -> [StackFrame] -> Either String Acc
evalExpr ctx (AstExpr p _ v) frames = 
    let set result = return $ Acc (Just result) ctx frames in
    case v of
        -- num, str and symb evaluate to themselves
        AstNum n -> set $ PzNum n
        AstStr s -> set $ PzStr s
        AstSymb symb -> set $  PzSymb symb

        -- replace identifier with corresponding value from ctx
        AstIdent ident ->
            case dictGet (PzSymb $ fromIdent ident) ctx of
                PzUnit -> Left $ "Error: Undefined identifier: " ++ show ident ++ "\n at: " ++ show p
                val -> set val

        -- push form on stack
        AstList k _ elems -> return $ Acc Nothing ctx $ Form p (toForm p k elems) : frames

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