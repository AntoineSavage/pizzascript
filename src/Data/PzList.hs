module Data.PzList (PzList(..), parser, unparse) where

import Data.List ( intercalate )
import Text.Parsec ( char, manyTill, try )
import Text.Parsec.String (Parser)

newtype PzList a
    = PzList [a]
    deriving (Show, Eq)

parser :: Parser () -> Parser a -> Parser (PzList a)
parser w p = PzList <$> (char '[' >> manyTill (w >> p) (try $ w >> char ']'))

unparse :: String -> (a -> String) -> PzList a -> String
unparse sep f (PzList xs) = "[" ++ intercalate sep (map f xs) ++ "]"