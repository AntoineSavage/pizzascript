module Data.PzStr (PzStr(..), parser, unparse) where

import Control.Monad
import Text.Parsec
import Text.Parsec.String (Parser)
import Utils

newtype PzStr =
    PzStr String
    deriving (Show, Eq)

parser :: Parser PzStr
parser = return $ PzStr ""

unparse :: PzStr -> String
unparse (PzStr s) = ""

{- TODOs

Escapes:
- `\xD` to `\xDDDDD` : one unicode hex codepoint between 0x0 and 0x10FFFF incl.

-}