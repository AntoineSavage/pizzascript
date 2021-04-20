module Ast.AstList (AstList(..), parser, unparse) where

import Data.List ( intercalate )
import Text.Parsec ( char, manyTill, try )
import Text.Parsec.String (Parser)

newtype AstList a
    = AstList [a]
    deriving (Show, Eq)

parser :: Parser () -> Parser a -> Parser (AstList a)
parser w p = AstList <$> (char '[' >> manyTill (w >> p) (try $ w >> char ']'))

unparse :: String -> (a -> String) -> AstList a -> String
unparse sep f (AstList xs) = "[" ++ intercalate sep (map f xs) ++ "]"