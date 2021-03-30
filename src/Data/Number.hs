module Data.Number (Number(..), parser) where

import Control.Monad ( liftM2 )
import Text.Parsec
import Text.Parsec.String (Parser)
import Utils

data Number
    = NumberInt Int
    | NumberDouble Double 
    deriving (Eq, Ord, Show, Read)

parser :: Parser Number
parser =
    let uint = many1 $ oneOf digits
        sint = liftM2 (++) (option "" $ string "-") uint
    in NumberInt . read <$> sint