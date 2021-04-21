module Main where

import Ast
import Text.Parsec.String

main :: IO ()
main = do
    mresult <- parseFromFile parser "example/lex.pz"
    case mresult of
        Left err -> print err
        Right result -> print result