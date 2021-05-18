{-# LANGUAGE LambdaCase #-}
module Data.Lst ( Lst(..), LstKind(..), getStart, getEnd, parseLst, parseMany, unparseLst, unparseMany ) where

import Control.Monad
import Text.Parsec
import Text.Parsec.String ( Parser )

data Lst a
    = Lst LstKind [a]
    deriving (Show, Eq, Ord)

data LstKind
    = KindList
    | KindDict
    | KindForm
    deriving (Show, Eq, Ord)

getStart :: LstKind -> Char
getStart = \case
    KindList -> '['
    KindDict -> '{'
    KindForm -> '('

getEnd :: LstKind -> Char
getEnd = \case
    KindList -> ']'
    KindDict -> '}'
    KindForm -> ')'

parseLst :: Parser () -> Parser a -> Parser (Lst a)
parseLst ign p = undefined

unparseLst :: (Maybe a -> String) -> Lst a -> String
unparseLst f (Lst k es) = undefined

parseMany :: Parser () -> Parser a -> Parser () -> Parser [a]
parseMany ign elem end = go [] where
    go acc = ign >> optionMaybe end >>= \case
        Just _ -> return $ reverse acc
        Nothing -> elem >>= go . (:acc)

unparseMany :: (Maybe a -> String) -> [a] -> String
unparseMany f = \case
    []     -> f Nothing
    (x:xs) -> f (Just x) ++ unparseMany f xs