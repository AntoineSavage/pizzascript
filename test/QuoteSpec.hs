module QuoteSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Exception
import Types.Nat
import Types.PzVal
import Types.PzValSpec
import Types.Symb
import Quote

spec :: Spec
spec = do
    quoteVsUnquoteSpec
    quoteSpec
    unquoteSpec

quoteVsUnquoteSpec :: Spec
quoteVsUnquoteSpec = describe "quote vs unquote" $ do
    it "composes quote and unquote into id" $ do
        property $ \v -> do
            unquote (quote v) `shouldBe` Right v
            quote <$> unquote (quote v) `shouldBe` Right (quote v)

quoteSpec :: Spec
quoteSpec = describe "quote" $ do
    it "rejects the unit type" $ do
        let v = PzUnit
        evaluate (quote v) `shouldThrow` errorCall ("Can only quote numbers, strings, symbols and lists\n was: " ++ show v)

    it "quotes numbers into themselves" $ do
        property $ \n -> do
            let v = PzNum n
            quote v `shouldBe` v

    it "quotes strings into themselves" $ do
        property $ \s -> do
            let v = PzStr s
            quote v `shouldBe` v

    it "quotes symbols into themselves but one-more-quoted" $ do
        property $ \n f ns -> do
            let v = PzSymb $ Symb n f ns
            quote v `shouldBe` (PzSymb $ Symb (S n) f ns)

    it "quotes lists into themselves with elements quoted recursively" $ do
        property $ \vs -> do
            let v = PzList vs
            quote v `shouldBe` (PzList $ map quote vs)

    it "rejects dictionaries" $ do
        property $ \(ArbDict d) -> do
            let v = PzDict d
            evaluate (quote v) `shouldThrow` errorCall ("Can only quote numbers, strings, symbols and lists\n was: " ++ show v)

    it "rejects functions" $ do
        property $ \(ArbDict d) f -> do
            let v = PzFunc d f
            evaluate (quote v) `shouldThrow` errorCall ("Can only quote numbers, strings, symbols and lists\n was: " ++ show v)

unquoteSpec :: Spec
unquoteSpec = describe "unquote" $ do
    it "rejects the unit type" $ do
        let v = PzUnit
        unquote v `shouldBe` Left ("Can only unquote numbers, strings, symbols and lists\n was: " ++ show v)

    it "unquotes numbers into themselves" $ do
        property $ \n -> do
            let v = PzNum n
            unquote v `shouldBe` Right v

    it "unquotes strings into themselves" $ do
        property $ \s -> do
            let v = PzStr s
            unquote v `shouldBe` Right v

    it "rejects quoted identifiers" $ do
        property $ \f ns -> do
            let v = PzSymb $ Symb Z f ns
            unquote v `shouldBe` Left ("Cannot unquote identifier: " ++ f:ns)

    it "unquotes symbols into themselves but one-less-quoted" $ do
        property $ \n f ns -> do
            let v = PzSymb $ Symb (S n) f ns
            unquote v `shouldBe` Right (PzSymb $ Symb n f ns)

    it "unquotes lists into themselves with elements unquoted recursively" $ do
        property $ \vs -> do
            let v = PzList vs
            unquote v `shouldBe` PzList <$> mapM unquote vs
   
    it "rejects dictionaries" $ do
        property $ \(ArbDict d) -> do
            let v = PzDict d
            unquote v `shouldBe` Left ("Can only unquote numbers, strings, symbols and lists\n was: " ++ show v)
   
    it "rejects functions" $ do
        property $ \(ArbDict d) f -> do
            let v = PzFunc d f
            unquote v `shouldBe` Left ("Can only unquote numbers, strings, symbols and lists\n was: " ++ show v)