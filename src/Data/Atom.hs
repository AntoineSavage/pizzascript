module Data.Atom (Atom, parser) where

import Text.Parsec
import Text.Parsec.String (Parser)

data Atom
    = AtomNone
    | AtomFalse
    | AtomTrue
    | Atom String

parser :: Parser Atom
parser = char ':' >> return AtomNone