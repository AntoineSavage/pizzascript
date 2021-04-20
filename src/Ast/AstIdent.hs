module Ast.AstIdent (AstIdent(..), AstIdentPart(..), parser, parsePart, unparse, unparsePart) where

import Control.Monad ( liftM2 )
import Data.List ( intercalate )
import Text.Parsec
import Text.Parsec.String (Parser)

data AstIdent
    = AstIdent AstIdentPart [AstIdentPart]
    deriving (Show, Eq)

data AstIdentPart
    = AstIdentPart Char String
    deriving (Show, Eq)

-- Parse / unparse ident
parser :: Parser AstIdent
parser = liftM2 AstIdent parsePart (many $ char '.' >> parsePart)

unparse :: AstIdent -> String
unparse (AstIdent p ps) = intercalate "." $ map unparsePart $ p : ps

-- Parse / unparse ident part
parsePart :: Parser AstIdentPart
parsePart = liftM2 AstIdentPart (letter <|> underscore) (many $ alphaNum  <|> underscore)

unparsePart :: AstIdentPart -> String
unparsePart (AstIdentPart f ns) = f : ns

underscore :: Parser Char
underscore = char '_'