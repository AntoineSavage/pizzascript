module Main where

import qualified Ast as A

import Data.Nat ( Nat )
import Eval (eval)
import Text.Parsec.String ( parseFromFile )

main :: IO ()
main = do
    putStrLn "Parsing..."
    mast <- parseFromFile A.parser "example/main.pz"
    case mast of
        Left err -> print err
        Right ast -> do
            putStrLn "Parsing finished"
            eval ast