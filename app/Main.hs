module Main where

import Ast ( parseAst )
import Eval ( evalAst )
import Text.Parsec.String ( parseFromFile )
import Types ( Ast(Ast) )

main :: IO ()
main = do
    mast <- parseFromFile parseAst "example/base.pz"
    case mast of
        Left err -> print err
        Right ast -> evalAst ast