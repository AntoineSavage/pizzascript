module Main where

import qualified AstSpec

main :: IO ()
main = do
    AstSpec.integrationTests
