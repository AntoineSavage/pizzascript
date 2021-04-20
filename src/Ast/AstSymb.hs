module Ast.AstSymb (AstSymb(..), parser, unparse) where

import Control.Monad ( liftM2 )
import qualified Ast.AstIdent as I
import Text.Parsec ( char, many1 )
import Text.Parsec.String (Parser)

data AstSymb
    = AstSymb Int I.AstIdent
    deriving (Show, Eq)

parser :: Parser AstSymb
parser = liftM2 AstSymb (length <$> many1 (char '\'')) I.parser

unparse :: AstSymb -> String
unparse (AstSymb n ident) = replicate (max 1 n) '\'' ++ I.unparse ident