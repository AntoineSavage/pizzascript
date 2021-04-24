module Ast.AstSymb (AstSymb(..), parser, unparse) where

import qualified Ast.AstIdent as I

import Control.Monad ( liftM2 )
import Data.Nat ( Nat, len, unlen )
import Text.Parsec ( char, many )
import Text.Parsec.String (Parser)

data AstSymb
    = AstSymb Nat I.AstIdent -- one implied leading quote
    deriving (Show, Eq)

parser :: Parser AstSymb
parser = liftM2 AstSymb (char '\'' >> len <$> many (char '\'')) I.parser

unparse :: AstSymb -> String
unparse (AstSymb n ident) = "'" ++ unlen '\'' n ++ I.unparse ident