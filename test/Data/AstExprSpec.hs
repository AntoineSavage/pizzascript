module Data.AstExprSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.AstExpr
import Data.Ident
import Data.Lst
import Data.LstSpec
import Data.NatSpec
import Data.Numb
import Data.NumbSpec
import Data.Str
import Data.StrSpec
import Data.Symb
import Data.Str
import TestUtils
import Text.Parsec

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
        property $ \n -> do
            parse (parseExpr ignore undefined) "tests" (unparseNumb n) `shouldBe` Right (AstNum n)

    it "parses str" $ do
        property $ \s -> do
            parse (parseExpr ignore undefined) "tests" (unparseStr s) `shouldBe` Right (AstStr s)

    it "parses ident" $ do
        property $ \ident -> do
            parse (parseExpr ignore undefined) "tests" (unparseIdent ident) `shouldBe` Right (AstIdent ident)

    it "parses symb" $ do
        property $ \n ident -> do
            parse (parseExpr ignore undefined) "tests" (unparseSymb $ Symb n ident) `shouldBe` Right (AstSymb $ Symb n ident)

    it "parses list" $ do
        let f Nothing = ""; f (Just e) = unparseExpr f e ++ " "
            g = parseExpr ignore g
        property $ \(Few es) -> do
            forM_ kinds $ \k -> do
                parse g "tests" (unparseLst f $ Lst k es) `shouldBe` Right (AstList $ Lst k es)

unparseExprSpec :: Spec
unparseExprSpec = describe "unparseExpr" $ do
    it "unparses num" $ do
        property $ \n -> do
            unparseExpr undefined (AstNum $ n) `shouldBe` unparseNumb n

    it "unparses str" $ do
        property $ \s -> do
            unparseExpr undefined (AstStr s) `shouldBe` unparseStr s

    it "unparses ident" $ do
        property $ \ident -> do
            unparseExpr undefined (AstIdent ident) `shouldBe` unparseIdent ident

    it "unparses symb" $ do
        property $ \n ident -> do
            unparseExpr undefined (AstSymb $ Symb n ident) `shouldBe` unparseSymb (Symb n ident)

    it "unparses list" $ do
        let f Nothing = ""; f (Just e) = unparseExpr f e
        property $ \(Few es) -> do
            forM_ kinds $ \k -> do
                unparseExpr f (AstList $ Lst k es) `shouldBe` unparseLst f (Lst k es)

-- Utils
ignore = spaces

instance Arbitrary AstExpr where arbitrary = arbDepth
instance ArbWithDepth AstExpr where
    arbWithDepth depth = oneof $
        [ AstNum . Numb <$> arbitrary
        , AstStr . Str <$> arbitrary
        , AstIdent <$> arbitrary
        , AstSymb <$> liftM2 Symb arbitrary arbitrary
        ] ++
        (if depth <= 0 then [] else
            [ fmap AstList $ liftM2 Lst arbitrary $ arbFew $ arbWithDepth $ depth-1
            ]
        )