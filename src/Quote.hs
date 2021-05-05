module Quote (quote, unquote) where

import Ast ( unparseExpr, unparseIdent, unparseList )
import Data.Nat ( Nat(..) )
import Types ( AstListKind(..), AstExpr(..), WithPos(..), Symb(..) )
import Utils ( symb, toForm )

quote :: WithPos AstExpr -> WithPos AstExpr
quote e@(WithPos p v) =
    let toExpr = WithPos p in
    case v of
        -- Numbers and strings quote as themselves
        AstNum _ -> e
        AstStr _ -> e

        -- Identifiers quote as symbols
        AstIdent ident ->
            toExpr $ AstSymb $ symb ident 

        -- Symbols quote as themselves with one more quote
        AstSymb (Symb n ident) ->
            toExpr $ AstSymb $ Symb (S n) ident

        -- Lists quote as forms prepended with list
        -- Dicts quote as forms prepended with dict
        -- Forms quote as list with elements quoted recursively
        AstList k es ->
            toExpr $ AstList KindList $ map quote $ toForm p k es

unquote :: WithPos AstExpr -> Either String (WithPos AstExpr)
unquote e@(WithPos p v) =
    let withPos = WithPos p in
    case v of
        -- Numbers and strings unquote as themselves
        AstNum _ -> return e
        AstStr _ -> return e

        -- Identifiers cannot be unquoted
        AstIdent ident ->
            Left $ "Unquote: unexpected identifier: " ++ unparseIdent ident

        -- Symbols with one quote unquote to identifiers
        -- Symbols with two or more quotes unquote to symbols with one less quote
        AstSymb (Symb n ident) ->
            return $ withPos $ case n of
                Z -> AstIdent ident
                (S n) -> AstSymb $ Symb n ident

        -- Lists unquote to forms with elements unquoted recursively
        -- Dicts and forms cannot be unquoted
        AstList k es ->
            case k of
                KindList -> withPos . AstList KindForm <$> mapM unquote es
                KindDict -> Left $ "Unquote: unexpected dictionary: " ++ unparseList KindDict unparseExpr es
                KindForm -> Left $ "Unquote: unexpected form: " ++ unparseList KindForm unparseExpr es