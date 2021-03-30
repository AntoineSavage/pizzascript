module Data.NumInt where

import Control.Monad ( liftM2 )
import Text.Parsec
import Text.Parsec.String (Parser)
import Utils

newtype NumInt = NumInt Int

parser :: Parser NumInt
parser = return $ NumInt 0