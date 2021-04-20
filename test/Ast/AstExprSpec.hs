module Ast.AstExprSpec where

import Test.Hspec
import Test.QuickCheck

import Ast.AstExpr
import qualified Ast.AstIdentSpec as AstIdent
import qualified Ast.AstListSpec as AstList
import qualified Ast.AstNumSpec as AstNum
import qualified Ast.AstStrSpec as AstStr
import qualified Ast.AstSymbSpec as AstSymb
import Control.Monad
import Data.Either
import Text.Parsec

spec :: Spec
spec = do
    parseVsUnparseSpec
    parseSpec
    unparseSpec

parseVsUnparseSpec :: Spec
parseVsUnparseSpec = describe "parse vs unparse" $ do
    it "moves" $ 1+1 `shouldBe` 2

parseSpec :: Spec
parseSpec = describe "parse" $ do
    it "moves" $ 1+1 `shouldBe` 2

unparseSpec :: Spec
unparseSpec = describe "unparse" $ do
    it "moves" $ 1+1 `shouldBe` 2

instance Arbitrary AstExpr where
    arbitrary = undefined