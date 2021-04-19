module Data.PzEval (PzEval(..), parser, unparse) where

import Data.List ( intercalate )
import Text.Parsec ( char, manyTill, try )
import Text.Parsec.String (Parser)

newtype PzEval a
    = PzEval [a]
    deriving (Show, Eq)

parser :: Parser () -> Parser a -> Parser (PzEval a)
parser w p = PzEval <$> (char '(' >> manyTill (w >> p) (try $ w >> char ')'))

unparse :: String -> (a -> String) -> PzEval a -> String
unparse sep f (PzEval xs) = "(" ++ intercalate sep (map f xs) ++ ")"