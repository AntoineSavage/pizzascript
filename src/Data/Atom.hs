module Data.Atom (Atom(..), parser, nameParser, fromName, toName, atomNameFirsts, atomNameNexts) where

import Control.Monad ( liftM2 )
import Text.Parsec ( many, oneOf, char )
import Text.Parsec.String (Parser)
import Utils ( digits, underscore, letterLower, letterUpper )

type AtomName = String

data Atom
    = AtomNone
    | AtomFalse
    | AtomTrue
    | Atom AtomName
    deriving (Eq, Ord, Show, Read)

parser :: Parser Atom
parser = char ':' >> (fromName <$> nameParser)

nameParser :: Parser AtomName
nameParser = liftM2 (:) (oneOf atomNameFirsts) (many $ oneOf atomNameNexts)

fromName :: String -> Atom
fromName atomName =
    case atomName of
        "none" -> AtomNone
        "false" -> AtomFalse
        "true" -> AtomTrue
        name -> Atom name

toName :: Atom -> String
toName atom =
    case atom of
        AtomNone -> "none"
        AtomFalse -> "false"
        AtomTrue -> "true"
        Atom name -> name

-- Internals, exposed for testing
atomNameFirsts :: String
atomNameFirsts = letterUpper ++ letterLower ++ [ underscore] 

atomNameNexts :: String
atomNameNexts = atomNameFirsts ++ digits