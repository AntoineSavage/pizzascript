module Pz.PzSymb (PzSymb(..), eval, uneval) where

import qualified Ast.AstIdent as I

import Ast.AstSymb (AstSymb(..))
import Data.Nat ( Nat )

data PzSymb
    = PzSymb Nat I.AstIdent
    deriving (Show, Eq)

eval :: AstSymb -> PzSymb
eval (AstSymb n ident) = PzSymb n ident

uneval :: PzSymb -> AstSymb
uneval (PzSymb n ident) = AstSymb n ident