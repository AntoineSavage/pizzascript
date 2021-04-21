module Ast.AstList (AstKind(..), AstList(..), getStart, getEnd, parser, unparse) where

import Data.List ( intercalate )
import Text.Parsec ( char, manyTill, try )
import Text.Parsec.String (Parser)

data AstList a
    = AstList AstKind String [a]
    deriving (Show, Eq)

data AstKind
    = AstKindList
    | AstKindDict
    | AstKindStruct
    | AstKindEval
    deriving (Show, Eq)

parser :: AstKind -> Parser String -> Parser a -> Parser (AstList a)
parser k doc p = AstList k "" <$> (char (getStart k) >> manyTill (doc >> p) (try $ doc >> char (getEnd k)))

unparse :: String -> (a -> String) -> AstList a -> String
unparse sep f (AstList k _ xs) = [getStart k] ++ intercalate sep (map f xs) ++ [getEnd k]

getStart :: AstKind -> Char
getStart AstKindList = '['
getStart AstKindDict = '{'
getStart AstKindStruct = '<'
getStart AstKindEval = '('

getEnd :: AstKind -> Char
getEnd AstKindList = ']'
getEnd AstKindDict = '}'
getEnd AstKindStruct = '>'
getEnd AstKindEval = ')'