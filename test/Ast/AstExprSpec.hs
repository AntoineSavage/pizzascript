module Ast.AstExprSpec where

import Test.Hspec
import Test.QuickCheck

import Ast
import Control.Monad
import Data.Ident
import Data.Symb
import Data.WithPos
import TestUtils
import Text.Parsec
import Types

spec :: Spec
spec = do
    parseExprVsUnparseExprSpec
    parseExprSpec
    unparseExprSpec

parseExprVsUnparseExprSpec :: Spec
parseExprVsUnparseExprSpec = describe "parseExpr vs unparseExpr" $ do
    it "composes parseExpr and unparseExpr into id" $ do
        let f Nothing = ""; f (Just e) = unparseExpr f e ++ " "
            g = parseExpr ignore g
        property $ \e -> do
            let s = unparseExpr f e
            parse g "tests" s `shouldBe` Right e
            unparseExpr f <$> parse g "tests" s `shouldBe` Right s

parseExprSpec :: Spec
parseExprSpec = describe "parseExpr" $ do
    it "parses num" $ do
        property $ \p n -> do
            parse (parseExpr ignore undefined) "tests" (unparseNum n) `shouldBe` Right (WithPos p $ AstNum n)

    it "parses str" $ do
        property $ \p s -> do
            parse (parseExpr ignore undefined) "tests" (unparseStr s) `shouldBe` Right (WithPos p $ AstStr s)

    it "parses ident" $ do
        property $ \p ident -> do
            parse (parseExpr ignore undefined) "tests" (unparseIdent ident) `shouldBe` Right (WithPos p $ AstIdent ident)

    it "parses symb" $ do
        property $ \p n ident -> do
            parse (parseExpr ignore undefined) "tests" (unparseSymb $ Symb n ident) `shouldBe` Right (WithPos p $ AstSymb $ Symb n ident)

    it "parses list" $ do
        let f Nothing = ""; f (Just e) = unparseExpr f e ++ " "
            g = parseExpr ignore g
        property $ \p (Few es) -> do
            forM_ kinds $ \k -> do
                parse g "tests" (unparseList k f es) `shouldBe` Right (WithPos p $ AstList k es)

unparseExprSpec :: Spec
unparseExprSpec = describe "unparseExpr" $ do
    it "unparses num" $ do
        property $ \p n -> do
            unparseExpr undefined (WithPos p $ AstNum n) `shouldBe` unparseNum n

    it "unparses str" $ do
        property $ \p s -> do
            unparseExpr undefined (WithPos p $ AstStr s) `shouldBe` unparseStr s

    it "unparses ident" $ do
        property $ \p ident -> do
            unparseExpr undefined (WithPos p $ AstIdent ident) `shouldBe` unparseIdent ident

    it "unparses symb" $ do
        property $ \p n ident -> do
            unparseExpr undefined (WithPos p $ AstSymb $ Symb n ident) `shouldBe` unparseSymb (Symb n ident)

    it "unparses list" $ do
        let f Nothing = ""; f (Just e) = unparseExpr f e
        property $ \p (Few es) -> do
            forM_ kinds $ \k -> do
                unparseExpr f (WithPos p $ AstList k es) `shouldBe` unparseList k f es

unparseElem' = unparseElem.Just

parseList' k = parse (parseList k ignore parseElem) "tests"
unparseList' k = unparseList k unparseElem

parseMany' = parse (parseMany spaces parseElem $ void $ char '$') "tests"
unparseMany' es = unparseMany unparseElem es