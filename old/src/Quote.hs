module Quote (quote, unquote, unparse) where

import Data.AstExpr ( AstExpr(..), unparseExpr )
import Data.Ident ( unparseIdent )
import Data.Lst ( Lst(..), LstKind(..), unparseLst )
import Data.Nat ( Nat(..) )
import Data.Symb ( Symb(Symb), symb )
import Utils ( Result, toForm )

quote :: AstExpr -> AstExpr
quote e =
    case e of
        -- Numbers and strings quote as themselves
        AstNum _ -> e
        AstStr _ -> e

        -- Identifiers quote as symbols
        AstIdent ident -> AstSymb $ symb ident

        -- Symbols quote as themselves with one more quote
        AstSymb (Symb n ident) -> AstSymb $ Symb (S n) ident

        -- Lists quote as forms prepended with list
        -- Dicts quote as forms prepended with dict
        -- Forms quote as list with elements quoted recursively
        AstList (Lst k es) -> AstList $ Lst KindList $ map quote $ toForm k es

unquote :: AstExpr -> Result AstExpr
unquote e =
    case e of
        -- Numbers and strings unquote as themselves
        AstNum _ -> return e
        AstStr _ -> return e

        -- Identifiers cannot be unquoted
        AstIdent ident ->
            Left $ "Unquote: unexpected identifier: " ++ unparseIdent ident

        -- Symbols with one quote unquote to identifiers
        -- Symbols with two or more quotes unquote to symbols with one less quote
        AstSymb (Symb n ident) ->
            return $ case n of
                Z -> AstIdent ident
                (S n) -> AstSymb $ Symb n ident

        -- Lists unquote to forms with elements unquoted recursively
        -- Dicts and forms cannot be unquoted
        AstList (Lst k es) ->
            case k of
                KindList -> AstList . Lst KindForm <$> mapM unquote es
                KindDict -> Left $ "Unquote: unexpected dictionary: " ++ unparseLst unparse (Lst KindDict es)
                KindForm -> Left $ "Unquote: unexpected form: " ++ unparseLst unparse (Lst KindForm es)

-- TODO Replace with pretty
unparse :: Maybe AstExpr -> String
unparse Nothing = ""
unparse (Just e) = unparseExpr unparse e