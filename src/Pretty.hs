{-# LANGUAGE LambdaCase #-}
module Pretty where

import Ast ( unparseExpr )
import Data.AstExpr ( AstExpr )
import Data.Ident ( Ident, unparseIdent )
import Data.Symb ( Symb, unparseSymb )
import Data.WithPos ( WithPos )
import Text.Parsec ( SourcePos )

identSize :: Int
identSize = 2

maxLineSize :: Int
maxLineSize = 120

class Pretty a where pretty :: Int -> a -> String
class PrettyWithPos a where prettyWithPos :: Int -> WithPos a -> String

-- Shared types
instance Pretty Ident where pretty _ = unparseIdent
instance Pretty Symb where pretty _ = unparseSymb
instance Pretty SourcePos where pretty _ = show
instance PrettyWithPos a => Pretty (WithPos a) where pretty = prettyWithPos

-- AST types
-- TODO Handle newlines and indent
instance PrettyWithPos AstExpr where
    prettyWithPos n = unparseExpr f where
        f Nothing = ""
        f (Just e) = unparseExpr f e

-- Value types
-- TODO