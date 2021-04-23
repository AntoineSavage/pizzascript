module AstSpec where

import Test.Hspec

import Ast
import Text.Parsec

integrationTests :: IO ()
integrationTests = do
    s <- readFile "example/ast.pz"
    hspec $ do
        describe "parse example/ast.pz" $ do
            it "composes parse and unparse into id" $ do
                unparse <$> parse parser "integrationTests" s `shouldBe` Right s
