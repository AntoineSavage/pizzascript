module QuoteSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Exception
--import Control.Monad
import Data.PzVal
import Data.PzValSpec
--import Data.Either
--import Data.Ident
--import Data.IdentSpec
--import Data.Lst
import Data.Nat
--import Data.NatSpec
import Data.NumbSpec
import Data.StrSpec
import Data.Symb
import Data.SymbSpec
import Quote
import TestUtils
--import Utils

spec :: Spec
spec = do
    quoteVsUnquoteSpec
    quoteSpec
    unquoteSpec

quoteVsUnquoteSpec :: Spec
quoteVsUnquoteSpec = describe "quote vs unquote" $ do
    it "composes quote and unquote into id" $ do
        property $ \(UnquoteValid v) -> do
            unquote (quote v) `shouldBe` v
            unquote (unquote $ quote $ quote v) `shouldBe` v
            unquote (unquote $ unquote $ quote $ quote $ quote v) `shouldBe` v

quoteSpec :: Spec
quoteSpec = describe "quote" $ do
    it "rejects the unit type" $ do
        let v = PzUnit
        evaluate (quote v) `shouldThrow` errorCall ("Value must be unevaluated before quoting: " ++ show v)

    it "converts numbers into themselves" $ do
        property $ \n -> do
            let v = PzNum n
            quote v `shouldBe` v

    it "converts strings into themselves" $ do
        property $ \s -> do
            let v = PzStr s
            quote v `shouldBe` v

    it "converts symbols into one-more-quoted symbols" $ do
        property $ \n f ns -> do
            quote (PzSymb $ Symb n f ns) `shouldBe` PzSymb (Symb (S n) f ns)

    it "converts lists into themselves with elements quoted recursively" $ do
        property $ \(UnquoteValids es) -> do
            quote (PzList es) `shouldBe` PzList (map quote es)

    it "rejects dictionaries" $ do
        property $ \(ArbDict d) -> do
            let v = PzDict d
            evaluate (quote v) `shouldThrow` errorCall ("Value must be unevaluated before quoting: " ++ show v)

    it "rejects functions" $ do
        property $ \(ArbDict d) f -> do
            let v = PzFunc d f
            evaluate (quote v) `shouldThrow` errorCall ("Value must be unevaluated before quoting: " ++ show v)

unquoteSpec :: Spec
unquoteSpec = describe "unquote" $ do
    it "rejects the unit type" $ do
        let v = PzUnit
        evaluate (unquote v) `shouldThrow` errorCall ("Value must be unevaluated before unquoting: " ++ show v)

    it "converts numbers into themselves" $ do
        property $ \n -> do
            let v = PzNum n
            unquote v `shouldBe` v

    it "converts strings into themselves" $ do
        property $ \s -> do
            let v = PzStr s
            unquote v `shouldBe` v

    it "rejects quoted identifiers" $ do
        property $ \(QuotedIdent s) -> do
            let v = PzSymb s
                Symb Z f ns = s
                r@(PzSymb (Symb _ _ _)) = unquote v -- reach unquoteSymb error
            evaluate r `shouldThrow` errorCall ("Quoted identifier cannot be unquoted: " ++ f:ns)

    it "converts quoted symbols into one-less-quoted symbols" $ do
        property $ \n f ns -> do
            unquote (PzSymb $ Symb (S n) f ns) `shouldBe` (PzSymb $ Symb n f ns)

    it "converts lists into themselves with elements unquoted recursively" $ do
        property $ \(UnquoteValids es) -> do
            let v = PzList es
            unquote v `shouldBe` (PzList $ map unquote es)
   
    it "rejects dictionaries" $ do
        property $ \(ArbDict d) -> do
            let v = PzDict d
            evaluate (unquote v) `shouldThrow` errorCall ("Value must be unevaluated before unquoting: " ++ show v)

    it "rejects functions" $ do
        property $ \(ArbDict d) f -> do
            let v = PzFunc d f
            evaluate (unquote v) `shouldThrow` errorCall ("Value must be unevaluated before unquoting: " ++ show v)

-- Utils
newtype UnquoteValid = UnquoteValid PzVal deriving (Show, Eq)
instance Arbitrary UnquoteValid where arbitrary = arbDepth
instance ArbWithDepth UnquoteValid where
    arbWithDepth depth = fmap UnquoteValid $ oneof $
        [ PzNum <$> arbitrary
        , PzStr <$> arbitrary
        , PzSymb <$> arbQuotedSymb
        ] ++
        ( if depth <= 0 then [] else
            [ PzList <$> do UnquoteValids es <- arbitrary; return es
            ]
        )

newtype UnquoteValids = UnquoteValids [PzVal] deriving (Show, Eq)
instance Arbitrary UnquoteValids where arbitrary = arbDepth
instance ArbWithDepth UnquoteValids where arbWithDepth depth = fmap UnquoteValids $ arbFew $ arbUnquoteValidWithDepth $ depth-1

arbUnquoteValid :: Gen PzVal
arbUnquoteValid = do UnquoteValid e <- arbitrary; return e

arbUnquoteValidWithDepth :: Int -> Gen PzVal
arbUnquoteValidWithDepth depth = do UnquoteValid e <- arbWithDepth depth; return e