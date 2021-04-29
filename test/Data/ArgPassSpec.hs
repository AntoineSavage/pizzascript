module Data.ArgPassSpec where

import Test.Hspec
import Test.QuickCheck

import Data.ArgPass
import Data.Symb

spec :: Spec
spec = do
    toSymbVsFromSymbSpec
    toSymbSpec
    fromSymbSpec

toSymbVsFromSymbSpec :: Spec
toSymbVsFromSymbSpec = describe "toSymb vs fromSymb" $ do
    it "composes toSymb and fromSymb into id" $ do
        property $ \(ArgPassSymb s) -> do
            toSymb <$> fromSymb s `shouldBe` Just s
            fromSymb <$> toSymb <$> fromSymb s `shouldBe` fromSymb <$> Just s

toSymbSpec :: Spec
toSymbSpec = describe "toSymb" $ do
    it "converts supported arg pass values" $ do
        toSymb Eval `shouldBe` symbEval
        toSymb Quote `shouldBe` symbQuote
        toSymb Unquote `shouldBe` symbUnquote
        toSymb DeepQuote `shouldBe` symbDeepQuote
        toSymb DeepUnquote `shouldBe` symbDeepUnquote

fromSymbSpec :: Spec
fromSymbSpec = describe "fromSymb" $ do
    it "fails for unsupported symbols" $ do
        property $ \s -> do
            fromSymb (symb $ "_" ++ s) `shouldBe` Nothing

    it "succeeds for supported symbols" $ do
        fromSymb symbEval `shouldBe` Just Eval
        fromSymb symbQuote `shouldBe` Just Quote
        fromSymb symbUnquote `shouldBe` Just Unquote
        fromSymb symbDeepQuote `shouldBe` Just DeepQuote
        fromSymb symbDeepUnquote `shouldBe` Just DeepUnquote

argPassSymbs =
    [ symbEval
    , symbQuote
    , symbUnquote
    , symbDeepQuote
    , symbDeepUnquote
    ]

newtype ArgPassSymb = ArgPassSymb Symb deriving (Show, Eq)
instance Arbitrary ArgPassSymb where
    arbitrary = ArgPassSymb <$> elements argPassSymbs
        