module Main where

import Ast ( parseAst )
import Eval ( exec, newAcc )
import Text.Parsec.String ( parseFromFile )
import Types ( Ast(Ast) )

main :: IO ()
main = do
    mast <- parseFromFile parseAst "example/main.pz"
    case mast of
        Left err -> print err
        Right ast -> exec $ newAcc ast