module QuoteSpec where

import Test.Hspec
import Test.QuickCheck

import Ast
import Data.Either
import Data.Ident
import Data.Nat
import Quote
import TestUtils
import Types
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
        property $ \p n -> do
            let e = WithPos p $ AstNum n
            quote e `shouldBe` e

    it "converts strings into themselves" $ do
        property $ \p s -> do
            let e = WithPos p $ AstStr s
            quote e `shouldBe` e

    it "converts identifiers into single-quoted symbols" $ do
        property $ \p i -> do
            quote (WithPos p $ AstIdent i) `shouldBe` WithPos p (AstSymb $ Symb Z i)

    it "converts symbols into one-more-quoted symbols" $ do
        property $ \p n ident -> do
            quote (WithPos p $ AstSymb $ Symb n ident) `shouldBe` WithPos p (AstSymb $ Symb (S n) ident)

    it "converts lists into 'list-prefixed lists" $ do
        property $ \p (Few es) -> do
            quote (WithPos p $ AstList KindList es) `shouldBe`
                WithPos p (AstList KindList $ map quote $ toForm p KindList es)

    it "converts dicts into 'dict-prefixed lists" $ do
        property $ \p (Few es) -> do
            quote (WithPos p $ AstList KindDict es) `shouldBe`
                WithPos p (AstList KindList $ map quote $ toForm p KindDict es)

    it "converts forms into lists" $ do
        property $ \p (Few es) -> do
            quote (WithPos p $ AstList KindForm es) `shouldBe`
                WithPos p (AstList KindList $ map quote es)

unquoteSpec :: Spec
unquoteSpec = describe "unquote" $ do
    it "converts numbers into themselves" $ do
        property $ \p n -> do
            let e = WithPos p $ AstNum n
            unquote e `shouldBe` Right e

    it "converts strings into themselves" $ do
        property $ \p s -> do
            let e = WithPos p $ AstStr s
            unquote e `shouldBe` Right e

    it "rejects identifiers" $ do
        property $ \p i -> do
            let e = WithPos p $ AstIdent i
            unquote e `shouldBe` Left ("Unquote: unexpected identifier: " ++ unparseIdent i)

    it "converts single-quoted symbols into identifiers" $ do
        property $ \p i -> do
            unquote (WithPos p $ AstSymb $ Symb Z i) `shouldBe` Right (WithPos p $ AstIdent i)

    it "converts two-or-more-quoted symbols into one-less-quoted symbol" $ do
        property $ \p n i -> do
            unquote (WithPos p $ AstSymb $ Symb (S n) i) `shouldBe` Right (WithPos p $ AstSymb $ Symb n i)

    it "converts lists into forms" $ do
        property $ \p (UnquoteValids es) -> do
            let list = AstList KindList es
                mactual = unquote $ WithPos p list
            isRight mactual `shouldBe` True
            mactual `shouldBe` (WithPos p . AstList KindForm <$> mapM unquote es)
   
    it "rejects dictionaries" $ do
        property $ \p (Few es) -> do
            let dictionary = AstList KindDict es
            unquote (WithPos p dictionary) `shouldBe`
                Left ("Unquote: unexpected dictionary: " ++ unparseList KindDict unparse es)

    it "rejects forms" $ do
        property $ \p (Few es) -> do
            let form = AstList KindForm es
            unquote (WithPos p form) `shouldBe`
                Left ("Unquote: unexpected form: " ++ unparseList KindForm unparse es)