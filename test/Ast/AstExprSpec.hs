module Ast.AstExprSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Ast.AstIdent as I
import qualified Ast.AstIdentSpec as IS
import qualified Ast.AstList as L
import qualified Ast.AstListSpec as LS
import qualified Ast.AstNum as N
import qualified Ast.AstNumSpec as NS
import qualified Ast.AstStr as St
import qualified Ast.AstStrSpec as StS
import qualified Ast.AstSymb as Sy
import qualified Ast.AstSymbSpec as SyS

import Ast.AstExpr
import Ast.AstListSpec (D(..))
import Control.Monad
import Data.Either as Either
import Text.Parsec
import Text.Parsec.Pos

spec :: Spec
spec = do
    parseVsUnparseSpec
    parseSpec
    unparseSpec

parseVsUnparseSpec :: Spec
parseVsUnparseSpec = describe "parse vs unparse" $ do
    it "composes parse and unparse into id" $ do
        property $ \e@(AstExpr pos d val) -> do
            parse (spaces >> parser doc d) "tests" (unparse e) `shouldBe` Right e
            unparse <$> parse (spaces >> parser doc d) "tests" (unparse e) `shouldBe` Right (unparse e)

parseSpec :: Spec
parseSpec = describe "parse" $ do
    it "parses num" $ do
        property $ \(D d) n -> do
            parse (parser doc d) "tests" (N.unparse n) `shouldBe` Right (AstExpr pos d $ AstValNum n)

    it "parses str" $ do
        property $ \(D d) s -> do
            parse (parser doc d) "tests" (St.unparse s) `shouldBe` Right (AstExpr pos d $ AstValStr s)

    it "parses ident" $ do
        property $ \(D d) i -> do
            parse (parser doc d) "tests" (I.unparse i) `shouldBe` Right (AstExpr pos d $ AstValIdent i)

    it "parses symb" $ do
        property $ \(D d) s -> do
            parse (parser doc d) "tests" (Sy.unparse s) `shouldBe` Right (AstExpr pos d $ AstValSymb s)

    it "parses list" $ do
        property $ \(D d) l -> do
            parse (parser doc d) "tests" (L.unparse unparse l) `shouldBe` Right (AstExpr pos d $ AstValList l)

unparseSpec :: Spec
unparseSpec = describe "unparse" $ do
    it "unparses num" $ do
        property $ \(D d) n -> do
            unparse (AstExpr pos d $ AstValNum n) `shouldBe` d ++ N.unparse n

    it "unparses str" $ do
        property $ \(D d) s -> do
            unparse (AstExpr pos d $ AstValStr s) `shouldBe` d ++ St.unparse s

    it "unparses ident" $ do
        property $ \(D d) i -> do
            unparse (AstExpr pos d $ AstValIdent i) `shouldBe` d ++ I.unparse i

    it "unparses symb" $ do
        property $ \(D d) s -> do
            unparse (AstExpr pos d $ AstValSymb s) `shouldBe` d ++ Sy.unparse s

    it "unparses list" $ do
        property $ \(D d) l -> do
            unparse (AstExpr pos d $ AstValList l) `shouldBe` d ++ L.unparse unparse l

-- Utils
doc = many space
pos = newPos "" 0 0

instance Arbitrary AstExpr where
    arbitrary = chooseInt (0, 3) >>= arbitraryOf

arbitraryOf depth = do
    -- Num: 0, Str: 1, Ident: 2, Symb: 3, List: 4
    D d <- arbitrary
    choice <- chooseInt (0, if depth <= 0 then 3 else 4)
    AstExpr pos d <$> case choice of
        0 -> AstValNum <$> arbitrary
        1 -> AstValStr <$> arbitrary
        2 -> AstValIdent <$> arbitrary
        3 -> AstValSymb <$> arbitrary
        4 -> AstValList <$> LS.arbitraryOf (arbitraryOf $ depth-1)