module Pz.PzSymb (PzSymb(..), eval, uneval) where

import Ast.AstSymb (AstSymb(..))
import qualified Ast.AstIdent as AstIdent

data PzSymb
    = PzSymb Int AstIdent.AstIdent
    deriving (Show, Eq)

eval :: AstSymb -> PzSymb
eval (AstSymb n ident) = PzSymb n ident

uneval :: PzSymb -> AstSymb
uneval (PzSymb n ident) = AstSymb n ident