module Data.PzNum (PzNum(..), parser, unparse, fromIntOrDouble) where

import Control.Monad ( liftM2 )
import Data.Maybe ( fromMaybe )
import Data.Ratio ( denominator, numerator )
import Text.Parsec
import Text.Parsec.String (Parser)

data PzNum
    = PzInteger Integer
    | PzDouble Double
    deriving (Show, Eq)

parser :: Parser PzNum
parser = do
    let uint = many digit
        sint = liftM2 (++) (option "" $ string "-") uint
        consOrNothing h t = optionMaybe $ liftM2 (:) h t
        emptyOr mx = fromMaybe "" mx
    i <- sint
    md <- consOrNothing (char '.') uint
    me <- consOrNothing (oneOf "eE") sint
    case (md, me) of
        (Nothing, Nothing) -> return $ PzInteger $ read i
        (_, _) -> return $ fromIntOrDouble PzInteger PzDouble $ read $ i ++ emptyOr md ++ emptyOr me

unparse :: PzNum -> String
unparse (PzInteger i) = show i
unparse (PzDouble d) = fromIntOrDouble show show d

fromIntOrDouble :: (Integer -> a) -> (Double -> a) -> Double -> a
fromIntOrDouble fromInt fromDouble d =
    let truncated = truncate d
    in if d == fromIntegral truncated
        then fromInt truncated
        else fromDouble d