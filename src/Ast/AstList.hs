module Ast.AstList (AstKind(..), AstList(..), getStart, getEnd, parser, parseElems, unparse, unparseElems) where

import Control.Monad ( void )
import Data.List ()
import Text.Parsec ( char, optionMaybe )
import Text.Parsec.String (Parser)

data AstList a
    = AstList AstKind String [a]
    deriving (Show, Eq)

data AstKind
    = AstKindList
    | AstKindDict
    | AstKindInv
    deriving (Show, Eq)

-- Parse/unparse list
parser :: AstKind -> Parser String -> (String -> Parser a) -> Parser (AstList a)
parser k doc p = char (getStart k) >> uncurry (flip $ AstList k) <$> parseElems doc p (void $ char $ getEnd k)

unparse :: (a -> String) -> AstList a -> String
unparse f (AstList k d xs) = [getStart k] ++ unparseElems d f xs ++ [getEnd k]

-- Parse/unparse elems
parseElems :: Parser String -> (String -> Parser a) -> Parser () -> Parser ([a], String)
parseElems doc elem end = do
    startDoc <- doc
    go [] startDoc
    where
        go acc d = do
            mend <- optionMaybe end
            case mend of
                Just _ -> return (reverse acc, d)
                Nothing -> do
                    e <- elem d
                    d' <- doc
                    go (e : acc) d'

unparseElems :: String -> (a -> String) -> [a] -> String
unparseElems d f xs = concatMap f xs ++ d

-- Start/end chars
getStart :: AstKind -> Char
getStart AstKindList = '['
getStart AstKindDict = '{'
getStart AstKindInv = '('

getEnd :: AstKind -> Char
getEnd AstKindList = ']'
getEnd AstKindDict = '}'
getEnd AstKindInv = ')'