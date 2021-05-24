{-# LANGUAGE LambdaCase #-}
module Data.Symb ( Symb(..), parseSymb, symb, unparseSymb ) where

import Data.Nat ( Nat(Z), len, unlen )
import Text.Parsec ( alphaNum, char, letter, (<|>), many )
import Text.Parsec.String ( Parser )

-- Symbols are implicitely quoted once
-- i.e. n=Z corresponds to one quote
data Symb
    = Symb Nat Char String
    deriving (Show, Eq, Ord)

symb :: String -> Symb
symb = \case
    []  -> error "Symbols must contain at least one character"
    c:s -> Symb Z c s

-- The output symbol is considered a quotation
-- i.e. n=Z corresponds to a quoted identifier
parseSymb :: Parser Symb
parseSymb = do
    n <- fmap len $ many $ char '\''
    let us = char '_'
    c <- letter <|> us
    s <- many $ alphaNum <|> us
    return $ Symb n c s

-- The input symbol is considered a quotation
-- i.e. n=Z corresponds to a quoted identifier
unparseSymb :: Symb -> String
unparseSymb (Symb n c s) = unlen n '\'' ++ c:s