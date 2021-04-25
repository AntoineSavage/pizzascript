module Main where

import qualified Ast as A

import Data.Nat ( Nat )
import Text.Parsec.String ( parseFromFile )

main :: IO ()
main = do
    mast <- parseFromFile A.parser "example/ast.pz"
    case mast of
        Left err -> print err
        Right ast -> writeFile "example/ast_out.pz" $ A.unparse ast