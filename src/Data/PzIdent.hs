module Data.PzIdent (PzIdent(..), parser, parseIdentPart, unparse, unparseIdentPart) where

import Text.Parsec
import Text.Parsec.String (Parser)

newtype PzIdent =
    PzIdent [PzIdentPart]
    deriving (Show, Eq)

newtype PzIdentPart
    = PzIdentPart String
    deriving (Show, Eq)

-- Parse / unparse ident
parser :: Parser PzIdent
parser = return $ PzIdent []

unparse :: PzIdent -> String
unparse (PzIdent ps) = ""

-- Parse / unparse ident part
parseIdentPart :: Parser PzIdentPart
parseIdentPart = return $ PzIdentPart ""

unparseIdentPart :: PzIdentPart -> String
unparseIdentPart (PzIdentPart s) = ""