module Main where

import Ast
import Text.Parsec.String

main :: IO ()
main = do
    mast <- parseFromFile parser "example/ast.pz"
    case mast of
        Left err -> print err
        Right ast -> writeFile "example/ast_out.pz" $ Ast.unparse ast