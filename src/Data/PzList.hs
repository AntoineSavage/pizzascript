module Data.PzList (PzList(..), parser, unparse) where

import Control.Monad
import Data.List
import Text.Parsec
import Text.Parsec.String (Parser)

newtype PzList a
    = PzList [a]
    deriving (Show, Eq)

-- Parse / unparse ident
parser :: Parser (PzList a)
parser = return $ PzList []

unparse :: PzList a -> String
unparse (PzList xs) = ""