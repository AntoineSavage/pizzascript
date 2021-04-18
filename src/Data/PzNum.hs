module Data.PzNum (PzNum(..), parser, unparse) where

import Control.Monad ( liftM2 )
import Data.Ratio ( denominator, numerator )
import Text.Parsec ( oneOf, string, option, many )
import Text.Parsec.String (Parser)
import Utils ( digits )

data PzNum =
    PzNum Integer Integer -- Numerator and Denominator
    deriving (Show, Eq)

parser :: Parser PzNum
parser = do
    let uint = many $ oneOf digits
        sint = liftM2 (++) (option "" $ string "-") uint
        dec = option "" $ liftM2 (++) (string ".") uint
        exp = option "" $ liftM2 (:) (oneOf "eE") sint
        s = concat <$> sequence [sint, dec, exp]
    ratio <- realToFrac . (read :: String -> Double) <$> s
    return $ PzNum (numerator ratio) (denominator ratio)

unparse :: PzNum -> String
unparse (PzNum n d) =
    truncateIfInt $ if d <= 0
        then 0
        else fromIntegral n / fromIntegral d

truncateIfInt :: Double -> String
truncateIfInt d =
    let truncated = truncate d
    in if d == fromIntegral truncated
        then show truncated
        else show d


{- TODOs

1) Specific-base positive integer literals:

0b01                # binary
0d0123456789        # decimal
0o01234567          # octal
0x0123456789ABCDEF  # hex (case-insensitive)
0x0123456789abcdef

-}