module Pz.PzSymbSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Ast.AstIdent as I

import Ast.AstSymb
import Ast.AstSymbSpec () -- instances
import Control.Monad
import Data.Either
import Pz.PzSymb
import Text.Parsec

spec :: Spec
spec = do
    evalVsUnevalSpec
    evalSpec
    unevalSpec

evalVsUnevalSpec :: Spec
evalVsUnevalSpec = describe "eval vs uneval" $ do
    it "composes eval and uneval" $ do
        property $ \ast pz -> do
            uneval (eval ast) `shouldBe` ast
            eval (uneval pz) `shouldBe` pz

evalSpec :: Spec
evalSpec = describe "eval" $ do
    it "converts pz symb" $ do
        property $ \(Positive n) ident -> 
            eval (AstSymb n ident) `shouldBe` PzSymb n ident

unevalSpec :: Spec
unevalSpec = describe "uneval" $ do
    it "converts ast symb" $ do
        property $ \(Positive n) ident -> do
            uneval (PzSymb n ident) `shouldBe` AstSymb n ident

instance Arbitrary PzSymb where
    arbitrary = eval <$> arbitrary