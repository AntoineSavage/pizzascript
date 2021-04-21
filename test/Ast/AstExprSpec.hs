module Ast.AstExprSpec where

import Test.Hspec
import Test.QuickCheck

import Ast.AstExpr
import qualified Ast.AstIdentSpec as AstIdentSpec
import qualified Ast.AstIdent as AstIdent
import qualified Ast.AstListSpec as AstListSpec
import qualified Ast.AstList as AstList
import qualified Ast.AstNumSpec as AstNumSpec
import qualified Ast.AstNum as AstNum
import qualified Ast.AstStrSpec as AstStrSpec
import qualified Ast.AstStr as AstStr
import qualified Ast.AstSymbSpec as AstSymbSpec
import qualified Ast.AstSymb as AstSymb

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
    it "composes parse and unparse into id" $ do
        property $ \e -> do
            parse (parser doc) "tests" (unparse sep e) `shouldBe` Right e
            unparse sep <$> parse (parser doc) "tests" (unparse sep e) `shouldBe` Right (unparse sep e)

parseSpec :: Spec
parseSpec = describe "parse" $ do
    it "parses num" $ do
        property $ \n -> do
            parse (parser doc) "tests" (AstNum.unparse n) `shouldBe` Right (AstExpr "" $ AstNum n)

    it "parses str" $ do
        property $ \s -> do
            parse (parser doc) "tests" (AstStr.unparse s) `shouldBe` Right (AstExpr "" $ AstStr s)

    it "parses ident" $ do
        property $ \i -> do
            parse (parser doc) "tests" (AstIdent.unparse i) `shouldBe` Right (AstExpr "" $ AstIdent i)

    it "parses symb" $ do
        property $ \s -> do
            parse (parser doc) "tests" (AstSymb.unparse s) `shouldBe` Right (AstExpr "" $ AstSymb s)

    it "parses list" $ do
        property $ \l -> do
            parse (parser doc) "tests" (AstList.unparse sep (unparse sep) l) `shouldBe` Right (AstExpr "" $ AstList l)

unparseSpec :: Spec
unparseSpec = describe "unparse" $ do
    it "unparses num" $ do
        property $ \n -> do
            unparse sep (AstExpr "" $ AstNum n) `shouldBe` AstNum.unparse n

    it "unparses str" $ do
        property $ \s -> do
            unparse sep (AstExpr "" $ AstStr s) `shouldBe` AstStr.unparse s

    it "unparses ident" $ do
        property $ \i -> do
            unparse sep (AstExpr "" $ AstIdent i) `shouldBe` AstIdent.unparse i

    it "unparses symb" $ do
        property $ \s -> do
            unparse sep (AstExpr "" $ AstSymb s) `shouldBe` AstSymb.unparse s

    it "unparses list" $ do
        property $ \l -> do
            unparse sep (AstExpr "" $ AstList l) `shouldBe` AstList.unparse sep (unparse sep) l

-- Utils
doc = many (char ' ')
sep = " "

instance Arbitrary AstExpr where
    arbitrary = chooseInt (0, 3) >>= arbitraryOf

arbitraryOf d = do
    -- Num: 0, Str: 1, Ident: 2, Symb: 3, List: 4
    choice <- chooseInt (0, if d <= 0 then 3 else 4)
    AstExpr "" <$> case choice of
        0 -> AstNum <$> arbitrary
        1 -> AstStr <$> arbitrary
        2 -> AstIdent <$> arbitrary
        3 -> AstSymb <$> arbitrary
        4 -> AstList <$> AstListSpec.arbitraryOf (arbitraryOf $ d-1)