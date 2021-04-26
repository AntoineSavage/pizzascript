module Ast.AstNum (AstNum(..), parser, unparse) where

import Control.Monad ( liftM2 )
import Data.Maybe ( fromMaybe )
import Text.Parsec
import Text.Parsec.String (Parser)

newtype AstNum
    = AstNum Double
    deriving (Show, Eq)

parser :: Parser AstNum
parser = do
    let uint = many1 digit
        sint = liftM2 (++) (option "" $ string "-") uint
        consOrEmpty h t = option "" $ liftM2 (:) h t
        dec = consOrEmpty (char '.') uint
        exp = consOrEmpty (oneOf "eE") sint
    AstNum . read . concat <$> sequence [sint, dec, exp]

unparse :: AstNum -> String
unparse (AstNum d) =
    let truncated = truncate d
    in if d == fromIntegral truncated
        then show truncated
        else show d