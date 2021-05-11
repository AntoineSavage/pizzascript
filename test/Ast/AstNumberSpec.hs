module Ast.AstNumberSpec where

import Test.Hspec
import Test.QuickCheck

import Ast
import Data.Either
import Text.Parsec

spec :: Spec
spec = do
    parseNumVsUnparseNumSpec
    parseNumSpec
    unparseNumSpec

parseNumVsUnparseNumSpec :: Spec
parseNumVsUnparseNumSpec = describe "parseNum vs unparseNum" $ do
    it "composes parseNum and unparseNum into identity" $ do
        property $ \n -> do
            let s = unparseNum n
            parse parseNum "tests" s `shouldBe` Right n
            unparseNum <$> parse parseNum "tests" s `shouldBe` Right s

parseNumSpec :: Spec
parseNumSpec = describe "parseNum" $ do
    it "rejects empty string" $ do
        isLeft (parse parseNum "tests" "") `shouldBe` True

    it "parses integers" $ do
        property $ \n -> do
            parse parseNum "tests" (show (n :: Int)) `shouldBe` Right (fromIntegral n)

    it "parses doubles with decimal part" $ do
        property $ \intPart (Positive decPart) -> do
            let _ = (intPart :: Integer, decPart :: Integer)
                d = read $ show intPart ++ "." ++ show decPart
            parse parseNum "tests" (show d) `shouldBe` Right d

    it "parses doubles with exponential part" $ do
        property $ \intPart (Positive decPart) expPart -> do
            let _ = (intPart :: Integer, decPart :: Integer, expPart :: Integer)
                s1 = show intPart ++ "." ++ show (decPart + 1) ++ "e" ++ show expPart
                s2 = show intPart ++ "." ++ show (decPart + 1) ++ "E" ++ show expPart
                d1 = read s1
                d2 = read s2
            parse parseNum "tests" s1 `shouldBe` Right d1
            parse parseNum "tests" s1 `shouldBe` Right d2

    it "parses doubles" $ do
        property $ \d -> do
            parse parseNum "tests" (show d) `shouldBe` Right d

unparseNumSpec :: Spec
unparseNumSpec = describe "unparseNum" $ do
    it "returns show<int> for any integer" $ do
        property $ \n -> do
            unparseNum (fromIntegral n) `shouldBe` show (n :: Int)

    it "returns show<double> for any non-integer" $ do
        property $ \d -> do
            unparseNum (d + 0.1) `shouldBe` show (d + 0.1)