module Main where

import Ast ( parseExpr, parseMany, ignore )
import Eval ( evalMany )
import Text.Parsec ( eof )
import Text.Parsec.String ( parseFromFile )

main :: IO ()
main = do
    let parser = parseMany ignore parse eof
        parse = parseExpr ignore parse
    mes <- parseFromFile parser "example/small.pz"
    case mes of
        Left err -> print err
        Right es -> evalMany es
