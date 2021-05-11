module Data.Ident ( Ident(..), parseIdent, unparseIdent ) where

import Control.Monad ( liftM2 )
import Text.Parsec ( alphaNum, char, letter, (<|>), many )
import Text.Parsec.String ( Parser )

newtype Ident
    = Ident String
    deriving (Show, Eq, Ord)

parseIdent :: Parser Ident
parseIdent =
    let us = char '_' in
    fmap Ident $ liftM2 (:) (letter <|> us) $ many $ alphaNum <|> us

unparseIdent :: Ident -> String
unparseIdent (Ident s) = s