module Ast.AstIdent (PzIdent(..), PzIdentPart(..), parser, parsePart, unparse, unparsePart) where

import Control.Monad ( liftM2 )
import Data.List ( intercalate )
import Text.Parsec
import Text.Parsec.String (Parser)

data PzIdent
    = PzIdent PzIdentPart [PzIdentPart]
    deriving (Show, Eq)

data PzIdentPart
    = PzIdentPart Char String
    deriving (Show, Eq)

-- Parse / unparse ident
parser :: Parser PzIdent
parser = liftM2 PzIdent parsePart (many $ char '.' >> parsePart)

unparse :: PzIdent -> String
unparse (PzIdent p ps) = intercalate "." $ map unparsePart $ p : ps

-- Parse / unparse ident part
parsePart :: Parser PzIdentPart
parsePart = liftM2 PzIdentPart (letter <|> underscore) (many $ alphaNum  <|> underscore)

unparsePart :: PzIdentPart -> String
unparsePart (PzIdentPart f ns) = f : ns

underscore :: Parser Char
underscore = char '_'