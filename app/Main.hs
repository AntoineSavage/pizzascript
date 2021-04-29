module Main where

import Ast ( parseAst )
import Eval ( evalMany )
import Text.Parsec.String ( parseFromFile )
import Types ( Ast(Ast) )

main :: IO ()
main = do
    mast <- parseFromFile parseAst "example/main.pz"
    case mast of
        Left err -> print err
        Right (Ast _ es) -> evalMany es