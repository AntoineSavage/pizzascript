module Main where

import qualified Ast as A

import Eval ( evalAst )
import Text.Parsec.String ( parseFromFile )

main :: IO ()
main = do
    mast <- parseFromFile A.parser "example/ast.pz"
    case mast of
        Left err -> print err
        Right ast -> evalAst ast