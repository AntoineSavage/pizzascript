module Data.Symb where

import Control.Monad ( liftM2 )
import Data.Ident ( Ident, parseIdent, unparseIdent )
import Data.Nat ( Nat(..), len, unlen )
import Text.Parsec ( alphaNum, char, letter, (<|>), many )
import Text.Parsec.String ( Parser )

data Symb
    = Symb Nat Ident
    deriving (Show, Eq, Ord)

symb :: Ident -> Symb
symb = Symb Z

parseSymb :: Parser Symb
parseSymb = liftM2 Symb (char '\'' >> len <$> many (char '\'')) parseIdent

unparseSymb :: Symb -> String
unparseSymb (Symb n ident) = "'" ++ unlen n '\'' ++ unparseIdent ident
