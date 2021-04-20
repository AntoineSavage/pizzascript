module Ast.AstEval (AstEval(..), parser, unparse) where

import Data.List ( intercalate )
import Text.Parsec ( char, manyTill, try )
import Text.Parsec.String (Parser)

newtype AstEval a
    = AstEval [a]
    deriving (Show, Eq)

parser :: Parser () -> Parser a -> Parser (AstEval a)
parser w p = AstEval <$> (char '(' >> manyTill (w >> p) (try $ w >> char ')'))

unparse :: String -> (a -> String) -> AstEval a -> String
unparse sep f (AstEval xs) = "(" ++ intercalate sep (map f xs) ++ ")"