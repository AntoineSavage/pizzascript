module Pz.PzSymb (PzSymb(..), fromAst, toAst) where

import Ast.AstSymb (AstSymb(..))
import qualified Pz.PzIdent as PzIdent

data PzSymb
    = PzSymb Int PzIdent.PzIdent
    deriving (Show, Eq)

fromAst :: AstSymb -> PzSymb
fromAst (AstSymb n ident) = PzSymb n $ PzIdent.fromAst ident

toAst :: PzSymb -> AstSymb
toAst (PzSymb n ident) = AstSymb n $ PzIdent.toAst ident