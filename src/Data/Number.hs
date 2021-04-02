module Data.Number (Number(..), unparse, parser) where

import Control.Monad ( liftM2 )
import Text.Parsec
import Text.Parsec.String (Parser)
import Utils

data Number
    = NumberInt Int
    | NumberFloat Double
    deriving (Eq, Ord, Show, Read)

unparse :: Number -> String
unparse number =
    case number of
        NumberInt n -> show n
        NumberFloat d -> show d

parser :: Parser Number
parser = do
    let uint = many1 $ oneOf digits
        sint = liftM2 (++) (option "" $ string "-") uint
        option_or_cons cp sp = option "" $ liftM2 (:) cp sp
    intPart <- sint
    decPart <- option_or_cons (char '.') uint
    expPart <- option_or_cons (oneOf "eE") sint
    return $ case (decPart, expPart) of
        ("", "") -> NumberInt $ read intPart
        (d, e) -> NumberFloat $ read $ intPart ++ decPart ++ expPart
