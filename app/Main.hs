module Main where

import Ast ( parseExpr, parseMany, ignore )
import Eval ( eval )
import Text.Parsec ( eof )
import Text.Parsec.String ( parseFromFile )

main :: IO ()
main = do
    let parser = parseMany ignore (parseExpr ignore) eof
    mast <- parseFromFile parser "example/base.pz"
    case mast of
        Left err -> print err
        Right ast -> eval ast