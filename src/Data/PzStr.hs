module Data.PzStr (PzStr(..), parser, unparse) where

import Control.Monad
import Text.Parsec
import Text.Parsec.String (Parser)

newtype PzStr =
    PzStr String
    deriving (Show, Eq)

parser :: Parser PzStr
parser = fmap PzStr $ char '"' >> manyTill anyChar (char '"')

unparse :: PzStr -> String
unparse (PzStr s) = "\"" ++ s ++ "\""

{- TODOs

Escapes:
- `\xD`, `\xDD`, ...,  `\xDDDDDD` : one unicode hex codepoint between 0x0 and 0x10FFFF incl.

-}