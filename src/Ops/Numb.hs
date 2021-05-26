module Ops.Numb ( parseNumb, unparseNumb ) where

import Control.Monad ( liftM2 )
import Text.Parsec ( char, digit, oneOf, string, many1, option )
import Text.Parsec.String ( Parser )
import Types.Numb ( Numb(..) )

parseNumb :: Parser Numb
parseNumb = do
    let uint = many1 digit
        sint = liftM2 (++) (option "" $ string "-") uint
        consOrEmpty h t = option "" $ liftM2 (:) h t
        dec = consOrEmpty (char '.') uint
        exp = consOrEmpty (oneOf "eE") sint
    Numb . read . concat <$> sequence [sint, dec, exp]

unparseNumb :: Numb -> String
unparseNumb (Numb d) =
    let truncated = truncate d
    in if d == fromIntegral truncated
        then show truncated
        else show d