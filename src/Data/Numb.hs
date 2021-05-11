module Data.Numb ( Numb(..), parseNumb, unparseNumb ) where

import Control.Monad ( liftM2 )
import Text.Parsec ( char, digit, oneOf, string, many1, option )
import Text.Parsec.String ( Parser )

newtype Numb
    = Numb Double
    deriving (Show, Eq, Ord)

parseNumb :: Parser Double
parseNumb = do
    let uint = many1 digit
        sint = liftM2 (++) (option "" $ string "-") uint
        consOrEmpty h t = option "" $ liftM2 (:) h t
        dec = consOrEmpty (char '.') uint
        exp = consOrEmpty (oneOf "eE") sint
    read . concat <$> sequence [sint, dec, exp]

unparseNumb :: Double -> String
unparseNumb d =
    let truncated = truncate d
    in if d == fromIntegral truncated
        then show truncated
        else show d