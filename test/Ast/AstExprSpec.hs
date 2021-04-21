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

import Ast.AstListSpec (D(..))
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
        property $ \e@(AstExpr d val) -> do
            parse (spaces >> parser doc d) "tests" (unparse e) `shouldBe` Right e
            unparse <$> parse (spaces >> parser doc d) "tests" (unparse e) `shouldBe` Right (unparse e)

parseSpec :: Spec
parseSpec = describe "parse" $ do
    it "parses num" $ do
        property $ \(D d) n -> do
            parse (parser doc d) "tests" (AstNum.unparse n) `shouldBe` Right (AstExpr d $ AstValNum n)

    it "parses str" $ do
        property $ \(D d) s -> do
            parse (parser doc d) "tests" (AstStr.unparse s) `shouldBe` Right (AstExpr d $ AstValStr s)

    it "parses ident" $ do
        property $ \(D d) i -> do
            parse (parser doc d) "tests" (AstIdent.unparse i) `shouldBe` Right (AstExpr d $ AstValIdent i)

    it "parses symb" $ do
        property $ \(D d) s -> do
            parse (parser doc d) "tests" (AstSymb.unparse s) `shouldBe` Right (AstExpr d $ AstValSymb s)

    it "parses list" $ do
        property $ \(D d) l -> do
            parse (parser doc d) "tests" (AstList.unparse unparse l) `shouldBe` Right (AstExpr d $ AstValList l)

unparseSpec :: Spec
unparseSpec = describe "unparse" $ do
    it "unparses num" $ do
        property $ \(D d) n -> do
            unparse (AstExpr d $ AstValNum n) `shouldBe` d ++ AstNum.unparse n

    it "unparses str" $ do
        property $ \(D d) s -> do
            unparse (AstExpr d $ AstValStr s) `shouldBe` d ++ AstStr.unparse s

    it "unparses ident" $ do
        property $ \(D d) i -> do
            unparse (AstExpr d $ AstValIdent i) `shouldBe` d ++ AstIdent.unparse i

    it "unparses symb" $ do
        property $ \(D d) s -> do
            unparse (AstExpr d $ AstValSymb s) `shouldBe` d ++ AstSymb.unparse s

    it "unparses list" $ do
        property $ \(D d) l -> do
            unparse (AstExpr d $ AstValList l) `shouldBe` d ++ AstList.unparse unparse l

-- Utils
doc = many space

instance Arbitrary AstExpr where
    arbitrary = chooseInt (0, 3) >>= arbitraryOf

arbitraryOf depth = do
    -- Num: 0, Str: 1, Ident: 2, Symb: 3, List: 4
    D d <- arbitrary
    choice <- chooseInt (0, if depth <= 0 then 3 else 4)
    AstExpr d <$> case choice of
        0 -> AstValNum <$> arbitrary
        1 -> AstValStr <$> arbitrary
        2 -> AstValIdent <$> arbitrary
        3 -> AstValSymb <$> arbitrary
        4 -> AstValList <$> AstListSpec.arbitraryOf (arbitraryOf $ depth-1)