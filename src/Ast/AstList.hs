module Ast.AstList (AstKind(..), AstList(..), getStart, getEnd, parser, parseElems, unparse, unparseElems) where

import Data.List ( intercalate )
import Text.Parsec ( char, manyTill, optionMaybe, try )
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

-- Parse/unparse list
parser :: AstKind -> Parser String -> Parser a -> Parser (AstList a)
parser k doc p = AstList k "" <$> (char (getStart k) >> manyTill (doc >> p) (try $ doc >> char (getEnd k)))

unparse :: String -> (a -> String) -> AstList a -> String
unparse sep f (AstList k _ xs) = [getStart k] ++ intercalate sep (map f xs) ++ [getEnd k]

-- Parse/unparse elems
parseElems :: Parser String -> (String -> Parser a) -> Parser Char -> Parser ([a], String)
parseElems doc elem end = do
    doc >>= fmap (mapFirst reverse) . go [] where
        mapFirst f (x, y) = (f x, y)
        go acc d = do
            mend <- optionMaybe end
            case mend of
                Just _ -> return (acc, d)
                Nothing -> elem d >>= \e -> doc >>= go (e : acc)

unparseElems :: String -> (a -> String) -> [a] -> String
unparseElems d f xs = concatMap f xs ++ d

-- Start/end chars
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