module QuoteSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.AstExpr
import Data.AstExprSpec
import Data.Either
import Data.Ident
import Data.IdentSpec
import Data.Lst
import Data.Nat
import Data.NatSpec
import Data.Numb
import Data.Str
import Data.Symb
import Quote
import TestUtils
import Utils

spec :: Spec
spec = do
    quoteVsUnquoteSpec
    quoteSpec
    unquoteSpec

quoteVsUnquoteSpec :: Spec
quoteVsUnquoteSpec = describe "quote vs unquote" $ do
    it "composes quote and unquote into id" $ do
        property $ \e1 (UnquoteValid e2) -> do
            quote <$> unquote (quote e1) `shouldBe` Right (quote e1)
            quote <$> unquote e2 `shouldBe` Right e2

quoteSpec :: Spec
quoteSpec = describe "quote" $ do
    it "converts numbers into themselves" $ do
        property $ \n -> do
            let e = AstNum $ Numb n
            quote e `shouldBe` e

    it "converts strings into themselves" $ do
        property $ \s -> do
            let e = AstStr $ Str s
            quote e `shouldBe` e

    it "converts identifiers into single-quoted symbols" $ do
        property $ \i -> do
            quote (AstIdent i) `shouldBe` AstSymb (Symb Z i)

    it "converts symbols into one-more-quoted symbols" $ do
        property $ \n ident -> do
            quote (AstSymb $ Symb n ident) `shouldBe` AstSymb (Symb (S n) ident)

    it "converts lists into 'list-prefixed lists" $ do
        property $ \(Few es) -> do
            quote (AstList $ Lst KindList es) `shouldBe`
                AstList (Lst KindList $ map quote $ toForm KindList es)

    it "converts dicts into 'dict-prefixed lists" $ do
        property $ \(Few es) -> do
            quote (AstList $ Lst KindDict es) `shouldBe`
                AstList (Lst KindList $ map quote $ toForm KindDict es)

    it "converts forms into lists" $ do
        property $ \(Few es) -> do
            quote (AstList $ Lst KindForm es) `shouldBe`
                AstList (Lst KindList $ map quote es)

unquoteSpec :: Spec
unquoteSpec = describe "unquote" $ do
    it "converts numbers into themselves" $ do
        property $ \n -> do
            let e = AstNum $ Numb n
            unquote e `shouldBe` Right e

    it "converts strings into themselves" $ do
        property $ \s -> do
            let e = AstStr $ Str s
            unquote e `shouldBe` Right e

    it "rejects identifiers" $ do
        property $ \i -> do
            let e = AstIdent i
            unquote e `shouldBe` Left ("Unquote: unexpected identifier: " ++ unparseIdent i)

    it "converts single-quoted symbols into identifiers" $ do
        property $ \i -> do
            unquote (AstSymb $ Symb Z i) `shouldBe` Right (AstIdent i)

    it "converts two-or-more-quoted symbols into one-less-quoted symbol" $ do
        property $ \n i -> do
            unquote (AstSymb $ Symb (S n) i) `shouldBe` Right (AstSymb $ Symb n i)

    it "converts lists into forms" $ do
        property $ \(UnquoteValids es) -> do
            let list = AstList $ Lst KindList es
                mactual = unquote list
            isRight mactual `shouldBe` True
            mactual `shouldBe` (AstList . Lst KindForm <$> mapM unquote es)
   
    it "rejects dictionaries" $ do
        property $ \(Few es) -> do
            let dictionary = AstList $ Lst KindDict es
            unquote dictionary `shouldBe`
                Left ("Unquote: unexpected dictionary: " ++ unparseLst unparse (Lst KindDict es))

    it "rejects forms" $ do
        property $ \(Few es) -> do
            let form = AstList $ Lst KindForm es
            unquote form `shouldBe`
                Left ("Unquote: unexpected form: " ++ unparseLst unparse (Lst KindForm es))

-- Utils
arbUnquoteValid :: Gen AstExpr
arbUnquoteValid = do UnquoteValid e <- arbitrary; return e

newtype UnquoteValid = UnquoteValid AstExpr deriving (Show, Eq)
instance Arbitrary UnquoteValid where arbitrary = arbDepth
instance ArbWithDepth UnquoteValid where
    arbWithDepth depth = fmap UnquoteValid $ oneof $
        [ AstNum . Numb <$> arbitrary
        , AstStr . Str <$> arbitrary
        , AstSymb <$> liftM2 Symb arbitrary arbitrary
        ] ++
        ( if depth <= 0 then [] else
            [ (AstList . Lst KindList) <$> arbFew arbUnquoteValid
            ]
        )

newtype UnquoteValids = UnquoteValids [AstExpr] deriving (Show, Eq)
instance Arbitrary UnquoteValids where arbitrary = UnquoteValids <$> arbFew arbUnquoteValid
