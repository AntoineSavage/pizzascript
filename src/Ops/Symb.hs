{-# LANGUAGE LambdaCase #-}
module Ops.Symb ( getNbrQuotes, parseSymb, quoteSymb, symb, unparseSymb, unquoteSymb ) where

import Ops.Nat ( len, unlen )
import Text.Parsec ( alphaNum, char, letter, (<|>), many )
import Text.Parsec.String ( Parser )
import Types.Nat ( Nat(..) )
import Types.Symb ( Symb(..) )

symb :: String -> Symb
symb = \case
    []  -> error "Symbols must contain at least one character"
    c:s -> Symb Z c s

getNbrQuotes :: Symb -> Int
getNbrQuotes (Symb n _ _) = go n where
    go Z = 0
    go (S k) = 1 + go k

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

-- The output symbol is considered a quotation
quoteSymb :: Symb -> Symb
quoteSymb (Symb n c s) = Symb (S n) c s

-- The input symbol is considered a quotation
-- i.e. n=Z corresponds to a quoted identifier
unquoteSymb :: Symb -> Symb
unquoteSymb (Symb n c s) = case n of
    Z -> error $ "Quoted identifier cannot be unquoted: " ++ c:s
    S n' -> Symb n' c s
