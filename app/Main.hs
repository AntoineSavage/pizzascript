module Main where

import qualified Ast as A

import Eval ( eval )
import Text.Parsec.String ( parseFromFile )

main :: IO ()
main = do
    mast <- parseFromFile A.parseAst "example/main.pz"
    case mast of
        Left err -> print err
        Right (A.Ast _ es) -> eval es