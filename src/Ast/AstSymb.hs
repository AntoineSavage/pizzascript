module Ast.AstSymb (PzSymb(..), parser, unparse) where

import Control.Monad ( liftM2 )
import qualified Ast.AstIdent as I
import Text.Parsec ( char, many1 )
import Text.Parsec.String (Parser)

data PzSymb
    = PzSymb Int I.PzIdent
    deriving (Show, Eq)

parser :: Parser PzSymb
parser = liftM2 PzSymb (length <$> many1 (char '\'')) I.parser

unparse :: PzSymb -> String
unparse (PzSymb n ident) = replicate (max 1 n) '\'' ++ I.unparse ident