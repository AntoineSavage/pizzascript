module Ast.AstNum (AstNum(..), parser, unparse, fromIntOrDouble) where

import Control.Monad ( liftM2 )
import Data.Maybe ( fromMaybe )
import Data.Ratio ( denominator, numerator )
import Text.Parsec
import Text.Parsec.String (Parser)

data AstNum
    = AstInteger Integer
    | AstDouble Double
    deriving (Show, Eq)

parser :: Parser AstNum
parser = do
    let uint = many1 digit
        sint = liftM2 (++) (option "" $ string "-") uint
        consOrNothing h t = optionMaybe $ liftM2 (:) h t
        emptyOr mx = fromMaybe "" mx
    i <- sint
    md <- consOrNothing (char '.') uint
    me <- consOrNothing (oneOf "eE") sint
    case (md, me) of
        (Nothing, Nothing) -> return $ AstInteger $ read i
        (_, _) -> return $ fromIntOrDouble AstInteger AstDouble $ read $ i ++ emptyOr md ++ emptyOr me

unparse :: AstNum -> String
unparse (AstInteger i) = show i
unparse (AstDouble d) = fromIntOrDouble show show d

fromIntOrDouble :: (Integer -> a) -> (Double -> a) -> Double -> a
fromIntOrDouble fromInt fromDouble d =
    let truncated = truncate d
    in if d == fromIntegral truncated
        then fromInt truncated
        else fromDouble d