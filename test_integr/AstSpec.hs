module AstSpec where

import Test.Hspec

import Ast
import Text.Parsec

integrationTests :: IO ()
integrationTests = do
    s <- readFile "example/ast.pz"
    hspec $ do
        describe "parse example/ast.pz" $ do
            it "composes parseAst and unparseAst into id" $ do
                unparseAst <$> parse parseAst "integrationTests" s `shouldBe` Right s
