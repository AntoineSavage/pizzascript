module Pz.PzSymbSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Ast.AstIdent as AstIdent
import qualified Pz.PzIdent as PzIdent

import Ast.AstSymb
import Ast.AstSymbSpec () -- instances
import Pz.PzSymb
import Control.Monad
import Data.Either
import Text.Parsec

spec :: Spec
spec = do
    fromAstVsToAstSpec
    fromAstSpec
    toAstSpec

fromAstVsToAstSpec :: Spec
fromAstVsToAstSpec = describe "fromAst vs toAst" $ do
    it "composes fromAst and toAst" $ do
        property $ \ast pz -> do
            toAst (fromAst ast) `shouldBe` ast
            fromAst (toAst pz) `shouldBe` pz

fromAstSpec :: Spec
fromAstSpec = describe "fromAst" $ do
    it "converts pz symb" $ do
        property $ \(Positive n) ident -> 
            fromAst (AstSymb n ident) `shouldBe` PzSymb n (PzIdent.fromAst ident)

toAstSpec :: Spec
toAstSpec = describe "toAst" $ do
    it "converts ast symb" $ do
        property $ \(Positive n) ident -> do
            toAst (PzSymb n $ PzIdent.fromAst ident) `shouldBe` AstSymb n ident

instance Arbitrary PzSymb where
    arbitrary = fromAst <$> arbitrary