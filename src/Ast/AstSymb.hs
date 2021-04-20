module Ast.AstSymb (AstSymb(..), parser, unparse) where

import qualified Ast.AstIdent as AstIdent

import Control.Monad ( liftM2 )
import Text.Parsec ( char, many1 )
import Text.Parsec.String (Parser)

data AstSymb
    = AstSymb Int AstIdent.AstIdent
    deriving (Show, Eq)

parser :: Parser AstSymb
parser = liftM2 AstSymb (length <$> many1 (char '\'')) AstIdent.parser

unparse :: AstSymb -> String
unparse (AstSymb n ident) = replicate (max 1 n) '\'' ++ AstIdent.unparse ident